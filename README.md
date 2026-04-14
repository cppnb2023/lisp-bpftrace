# bpftrace-dsl 监控框架

一个用 Common Lisp 编写的 **bpftrace DSL** 及监控框架，允许你以面向对象的方式定义探针（probe）、生成 bpftrace 脚本、运行并解析输出，并通过 **规则（rule）** 系统灵活处理监控数据。

## 功能特性

- **DSL 构造 bpftrace 脚本**  
  通过 Lisp 宏和函数生成 `printf` 格式的 bpftrace 代码，支持 `:printf`、`:probe`、`:progn` 等语义。

- **监控类（monitor）**  
  基于 `monitor-base` 或自定义子类定义监控器，每个监控器拥有唯一的 `idx`，负责：
  - 生成对应的 bpftrace 代码片段（`generate-bpftrace-code`）
  - 解析 bpftrace 输出的数据（`read-information`）
  - 执行自定义钩子（`solve` 遍历 `hook-hash`）

- **规则系统（rule）**  
  将多个监控器组合成一个规则，当监控器收到数据时自动触发对应的回调函数，实现解耦的事件驱动逻辑。

- **集成 bpftrace 进程管理**  
  `exec-monitors` 启动 bpftrace 子进程，实时读取输出并分发给对应的监控器，自动调用规则。

- **辅助工具**  
  通用宏（`aif`、`awhen`、`do-stage` 等）、哈希表/列表处理、字符串格式化等。

## 系统要求

- Common Lisp 实现：**SBCL**（代码中使用了 `sb-ext:run-program`）
- **bpftrace** 命令（需在 `PATH` 中）
- Linux 内核支持 BPF（通常需要 root 权限运行生成的脚本）

## 安装

1. 将本项目的所有 Lisp 文件加载到你的 Lisp 系统中（例如通过 ASDF 或直接 `load`）。
2. 确保系统已安装 `bpftrace`：
   ```bash
   sudo apt install bpftrace   # Debian/Ubuntu
   ```

## 快速开始

以下示例展示如何定义一个监控 `sys_enter_execve` 探针的监控器，并创建一个规则打印进程的 ppid 和 pid。

```lisp
(in-package :main)

;; 1. 创建监控器实例
(defparameter *my-monitor*
  (make-monitor 'monitor-base
                (:tracepoint "syscalls" "sys_enter_execve")))

;; 2. 定义规则：当监控器收到数据时，打印 ppid 和 pid
(defparameter *my-rule*
  (make-rule ((*my-monitor*))
             (monitor)
             (with-monitor (monitor)
               (format t "ppid: ~a  pid: ~a~%"
                       (get-member :ppid)
                       (get-member :pid)))))

;; 3. 安装规则、生成 bpftrace 脚本、执行监控
(defun main ()
  (install-rule *my-rule*)
  (with-open-file (stream "/tmp/a.bt" :direction :output :if-exists :supersede)
    (add-monitors stream *my-monitor*))
  (exec-monitors "/tmp/a.bt"))

(main)
```

运行时，每次执行 `execve` 系统调用，终端就会输出类似：
```
ppid: 1234  pid: 5678
```

## 详细说明

### 1. 监控器（Monitor）

#### 基类 `monitor-base`
- **槽**：
  - `probe` : 探针字符串（例如 `"tracepoint:syscalls:sys_enter_execve"`）
  - `member-hash` : 存储解析后的数据（键值对）
  - `hook-hash` : 存储规则回调（键为规则对象，值为函数）
- **关键方法**：
  - `(generate-bpftrace-code monitor)` → 返回 bpftrace 代码字符串，默认输出 `:u32 "ppid" :u32 "pid" :u32 "tid" :u64 "nsecs"`。
  - `(read-information monitor plist)` → 将 bpftrace 输出的属性列表存入 `member-hash`。
  - `(solve monitor)` → 遍历 `hook-hash` 并调用所有回调。

#### 自定义监控器
使用 `defmonitor` 宏定义子类，并重写 `generate-bpftrace-code` 方法：

```lisp
(defmonitor my-exec-monitor (monitor-base)
  ((extra-slot :initarg :extra :reader get-extra)))
  (:bpftrace
   (:printf :u32 "ppid" :u32 "pid" :string "comm")))
```

### 2. 规则（Rule）

- **创建规则**：`(make-rule ((monitor1 ...) (monitor2 ...)) (monitor) (body))`  
  参数列表中的每个 `(monitor)` 表示当该监控器收到数据时，执行后面的 `body`。  
  未指定自定义 body 时使用默认 body（即 `(solve rule)`）。
- 注意可以在规则上定义规则进行嵌套，而且没有写规则成环限制。
- **安装/卸载**：`(install-rule rule)` 将规则的回调注册到每个监控器的 `hook-hash` 中；`(uninstall-rule rule)` 则移除。
- **规则触发**：当监控器执行 `solve` 时，会调用所有已注册的回调，并将监控器实例作为参数传入。

### 3. bpftrace DSL

- **`:printf`**：生成 `printf("(:hash idx ...)\n", args...)` 格式的字符串。
- **`:probe`**：生成 `probe { ... }` 块。
- **`:progn`**：生成 `{ ... }` 块，自动添加分号。
- **`bpftrace-code`**：顶层宏，内部可使用 `:printf`、`:probe`、`:progn`。
- 由于输出字符串面临性能，转义，解析问题，所以不推荐直接输出字符串（代码已经注释）。

示例：
```lisp
(bpftrace-code
  (:probe "kprobe:do_nanosleep"
    (:printf 1 :u64 "arg1" :u32 "arg2"))))
```

生成：
```
kprobe:do_nanosleep{printf("(:hash 1 arg1:%ld arg2:%u)\n", arg1, arg2);}
```

### 4. 进程管理

- **`add-monitors`**：将监控器写入脚本文件。
- **`exec-monitors`**：启动 `bpftrace` 进程，逐行读取输出，解析 `(:hash idx ...)` 格式的 plist，找到对应的监控器，调用 `read-information` 和 `solve`。

## API 参考

### 包 `generic`
提供通用工具宏/函数：
- `aif`, `awhen`, `aunless` – 带有 `it` 变量的条件宏
- `do-stage`, `do-list-stage`, `do-times-stage`, `do-plist-stage` – 流程控制
- `strcat`, `array-last`, `plist-into-hash` 等

### 包 `bpftrace-dsl`
- **宏**：
  - `bpftrace-code (&body body)` – DSL 入口
  - `with-write-bpftrace ((stream) &body body)` – 将代码写入流
- **函数**：
  - `bpftrace-printf (idx &rest plist)` → 字符串
  - `bpftrace-progn (&rest exprs)` → 字符串
  - `bpftrace-probe (probe &rest exprs)` → 字符串

### 包 `monitor-base`
- **类**：`monitor-base`
- **宏**：
  - `defmonitor (class-name parents members &body body)` – 定义新监控器类
  - `make-monitor (class-name probe)` – 创建实例，支持 `:filter`、`:kprobe`、`:tracepoint` 语法糖
- **函数**：
  - `add-monitors (stream &rest monitors)` – 写入脚本
  - `exec-monitors (file)` – 执行并处理输出

### 包 `rule`
- **类**：`rule`
- **宏**：`make-rule ((&rest monitor-or-rules) &body default-rule)` – 创建规则
- **函数**：`install-rule`、`uninstall-rule`

## 注意事项

- **权限**：bpftrace 通常需要 root 权限，请以 `sudo` 运行你的 Lisp 程序，或设置合适的 capabilities。
- **输出格式**：框架期望 bpftrace 输出的每行均为 `(:hash <idx> <key1> <value1> ...)` 形式的 plist。自定义监控器时请确保 `generate-bpftrace-code` 生成的 `printf` 与之匹配。
- **性能**：每收到一个事件都会触发 Lisp 侧的 `read-information` 和 `solve`，高频事件（如 `kprobe`）可能带来较大开销。
- **错误处理**：若 bpftrace 输出行无法解析为 plist 或 `:hash` 对应的监控器不存在，会打印错误并继续。

## 扩展与定制

- 继承 `monitor-base` 并重写 `read-information` 可实现自定义数据解析逻辑。
- 在规则回调中可调用 `(get-member monitor :key)` 获取数据，或调用 `(get-hook-hash monitor)` 动态管理其他钩子。
- 修改 `*monitor-hash*` 和 `*idx*` 的绑定可实现多租户或动态加载/卸载监控器。

