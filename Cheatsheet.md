## 函数
##### 向量生成与操作
* **seq(from, to, by = 步长, length.out = 指定长度):** 生成自定义序列
* **seq_len(n):** 生成从1到n的整数序列，可以安全处理 **n=0**，优于 **1: n**
* **seq_along(x):** 返回 1: length(x) 的整数向量，可以安全处理 **length(x)=0**，优于 **1: length(x)**
* **rep():** 重复元素生成向量
* **replicate():** 重复执行代码(函数)
* **sort():** 排序
* **rev(x):** 反转向量
* **table(v)**: 统计频次
* **unique(table, by, fromLast = TRUE):** 去重
	* table: 输入数据
	* by = c(...): 根据哪几列判断重复
	* fromLast = TRUE: 从后往前看，保留最后一条
* **head(x, n):** 取前 n 个元素
* **tail(x, n):** 取后 n 个元素
* **diff(x):** 相邻元素的差
* **cumsum(x):** 累积求和
* **order(...):** 返回排序后的索引
	* 全部升序：order(x, y, z, ...)
	* 全部降序：order(-x, -y, -z, ...)
	* 混合排序(升序+降序)：order(x, -y, z, ...)
* **rank(x):** 返回每个元素的排名
* **anyDuplicated():** 判断是否有重复, 返回**第一个重复元素的位置**（如果没有重复则返回 `0`）
	* 用法: if (anyDuplicated(...)) { stop("...")} 其中重复返回0，即 **FALSE**，不重复则>0，**TRUE**

##### 查找与索引
* **which():** 返回满足条件的**索引**，返回值是整数向量
* **which.min() / which.max():** 找到最小 / 大值的位置索引
	* 如果有平局, 返回第一个（任意选一个）
* **match(x, table):** 返回 x 在 table 中首次出现的位置索引
	* 示例: match("A", LETTERS[1:3]) ---> 1
*  **Filter(f, x):**
	* **f:** 一个**判断函数**，返回 TRUE 或 FALSE
	* **x:** 一个列表或向量
	* 返回 **x** 中所有使 **f** 返回 **TRUE** 的元素
	* z.B: Filter(function(x) x > 3, c(1, 2, 3, 4, 5))

##### 逻辑判断
* **if() / while():** 条件判断，注意如果是表示 "**或**" 要使用 "|**|**" !!!
* **all(logical_vector):** 是否全部为 TRUE，返回逻辑值
* **any(logical_vector):** 是否存在 TRUE，返回逻辑值
* **identical():** 检查两个对象是否完全相同
* **ifelse(test, yes, no):** 向量化的条件选择 (z.B: ifelse(c(TRUE, FALSE, TRUE), "Yes", "No") => "Yes" "No"  "Yes")
* **exists():** 用于检查变量/对象是否存在

##### 集合运算
* **union(A, B):** 并集
* **intersect(A, B):** 交集
* **setdiff(A, B):** 差集（所有列名-要删除的列名=保留的列名）
* **%in%:** 判断是否在集合中
	* 示例: c(1,2,5) %in% 1:3 → c(TRUE, TRUE, FALSE)
* **CJ(x, y, unique):** **C**ross **J**oin(交叉连接), 生成所有可能的组合
	* unique = TRUE: 只取唯一值

##### 数学计算
* **sqrt():** 计算平方根公式
* **round():** 不带参数，四舍五入到整数，等价于 round(x, 0)
* **abs():** 计算绝对值
* **frollmean():** 计算滚动平均
* **sum() / mean() / max() / min():** 基本统计

##### 随机数生成
* **runif(n, min, max):** 生成**均匀分布**的随机数 (小数)
* **.Random.seed:** 随机数生成器的状态(**全局变量用 <<-**)
	* 首次使用 RNG 前可能不存在，需要使用 runif() 来确保它存在
* **sample(n, size):** 通用抽样，输入可以是任意向量
* **sample.int(n, size):** 整数抽样，输入只能是整数 (n 很大时使用)
* **rnorm(x, mean, sd)**: 生成正态分布随机数
	* x: 生成数量
	* mean: 均值
	* sd: 标准差

##### 分组与分类
* **cut(x, breaks, labels, include.lowest, right, ordered_result):** 
	* **x:** 需要分类的数据
	* **breaks:** 分割线 (z.B: breaks = c(0, cutoffs, Inf))
	* **labels:** 分组标记
	* **include.lowest:** 逻辑值，是否将最左端的断点包含在第一个区间内
	* **right:** 逻辑值，区间是否右闭
	* **ordered_result:** 逻辑值，结果是否是有序因子
* **tapply(X, INDEX, FUN):** 对数据按分组应用函数
	* **X:** 要处理的数据
	* **INDEX:** 分组依据
	* **FUN:** 应用到每组的函数

##### Apply 家族
* **apply(X, MARGIN, FUN, ...):**
	* **X:** 矩阵
	* **MARGIN:** 1 表示按行操作，2 表示按列操作
	* **FUN:** 对每一行/列应用的函数
	* **...:** 传递给 **FUN** 的参数
* **lapply():** 对列表每个元素应用函数，结果返回 **列表**
* **sapply(X, FUN):** 类似 lapply，简化输出为 **向量/矩阵**
* **vapply(X, FUN, FUN.VALUE...) 用法：**
	* **X:** 要遍历的对象
	* **FUN:** 应用到每个元素的函数
	* **FUN.VALUE:** 返回值的类型和模版
		* FUN.VALUE = **TRUE**，说明要求每次返回一个**逻辑值**
		* FUN.VALUE = **0** 是 FUN.VALUE = numeric(1)的缩写，表示返回值是**单个数值**
		* FUN.VALUE = **""** 表示返回**单个字符串**， 等价于character(1)
* **mapply(FUN, ...):** 多参数并行处理
* **tapply(X, INDEX, FUN):** 按分组应用函数

##### 类型判断和转换
* **unlist():** 将列表转换为向量
	* 使用原因: sprintf() 期望向量，不是列表
* **as.list():** 将向量转换为列表
* **is.numeric() / is.character() / is.logical():** 类型判断
* **as.numeric() / as.character() / as.logical():** 类型转换
* **is.na(x):** 判断是否为 NA
* **is.null(x):** 判断是否为 NULL (**!!!重要**)
* **names(x):** 获取/设置名称
* **unname(x):** 移除名称
* **setNames(x, names):** 设置名称

##### 缺失值处理
* **na.omit():** 移除数据中的 **NA** 值，对于 **df** 和 **db**，删除任何列含 **NA** 的整行
* **is.na(x):** 判断是否为 NA

##### 函数调用与控制
* **do.call(what, args):** 将列表元素作为参数调用函数
	* **what:** 要调用的函数
	* **args:** 参数列表(必须是列表)
* **tryCatch():** 错误捕获 tryCatch({ 尝试执行的代码 }, error = function(e) { 错误处理 })
	* `error = function(e) { ... }` 里面是一个**新的函数环境**，在里面赋值不会影响外面的 `result`，所以报错返回结果应在 tryCatch 外部先定义好
	* `error = function(e) { return(e$message) }`可以得到报错信息(字符串) 
	* `e$message` 等价于 `conditionMessage(e)`
* **errorCondition(message, class):**
	* **message:** 错误消息
	* **class:** 错误类别
* **stop("message"):** 抛出错误
* **warning(message):** 抛出警告
* **return() / return(invisible(NULL):** 函数要求忽略返回值时使用

##### 调试与性能
* **system.time(expr):** 测量表达式执行所需的时间
* **print(x):** 打印对象 (自动换行)
* **cat(...):** 打印输出（需手动加 \n 换行）
	* cat("...\n") 或 cat(..., "\n")


## 循环
* **repeat()**: **无限循环**结构，必须通过`break` 或 `return` 来退出
	* 使用repeat()的原因：不知道**迭代次数**(否则**for**循环)，没有**条件**(否则**while**循环)
	* **z.B:**
```r
rngManip <- function(f) {
  # your code
  assertFunction(f)
  # 暴力搜索：尝试不同的种子 seed <- 0
  seed <- 0
  repeat {
    set.seed(seed)
    # 检查连续调用 10 次是否全为 "H"
    results <- replicate(10, f())
    if (all(results == "H")) {
	  # 找到了！重新设置这个种子后返回
      set.seed(seed)
      # 要求忽略返回值
      return()
    }
    seed <- seed + 1
  }
}
```

## 字符串
* **nchar(x):** 返回字符串长度
* **trimws():** 去除前后空格
* **gsub(x, y, expr)**: 可以删除或替换**任何位置**的内容，这里用""替换空格
	* 处理位置: 首、尾、中间任何地方
	* 和 **trimws()** 的区别: 
		* trimws() 只删除字符串**首尾**的空格，功能固定，专门去空格
* **toupper(x)**: 字符串全部转换为大写
* **tolower(x)**: 字符串全部转换为小写
* **strsplit(x, split,...)**: 按 split 切分字符串(""空字符串 = 每个字符分割)
	* strsplit() 返回**列表**，`[[1]]`返回**字符串向量**
* **sprintf(fmt, ...):**
	* **fmt:** 格式化字符串(包含占位符)
		* 常用占位符: 
			* %s: 字符串 (**万能占位符!!!通常都用这个**)
			* %d: 整数 (z.B: sprintf("%d of %d", 1, 10) → "1 of 10")
			* %f: 固定小数格式 ("0.000123")
			* %e: 科学计数法 ("1.230000e-04")
			* %g: 自动选择固定小数或科学计数法，取更短的那个 ("0.000123")
			* %.2f: 保留2位小数的浮点数
			* `%%` 表示输出一个 %符号
			* \n: 换行符（需要 cat() 解析）
	* **... :** 要插入的值
* **substr(x, start, stop):** 
	* 单个字符串: s <- "banana" --- substr(s, 1, 3) => "ban"
	* 多个字符串: substr(c("apple", "banana"), 1, 3) => "app" "ban"
	* **边界情况:**
		* stop 超出长度：自动截断到末尾 --- substr(s, 3, 100)  => "nana"
		* start 超出长度：返回空字符串 --- substr(s, 10, 12) => ""
		* start > stop：返回空字符串 --- substr(s, 4, 2) => ""
* **paste():** 将多个字符串拼接在一起（**有空格**）
	* sep: 元素之间的分隔符
	* collapse: 将向量合并为单个字符串的分隔符
* **paste0():** 将多个字符串拼接在一起（**无空格**），等价于 paste(..., sep = "")


## 正则表达式
* **字符:** 
	* `[[:alnum:]]` = `[A-Za-z0-9]` 匹配字母或数字
	* 字母: 
		* `[[:alpha:]]` = `[A-Za-z]` =  `\\w` 匹配任意一个字母
		* `[a-z]`: 匹配 a-z 的任意字母
		* `[abc]`: 匹配 a.b 或 c 中的任意一个
	* 数字: `[[:digit:]]` = `[0-9]` = `\\d` 匹配任意一个数字
* **量词:**
	* **`*`**: 重复0次或多次
	* **`+`**：重复1次或多次
	- **`?`**：重复0次或1次
	- **`{n}`**：精确重复n次
	- **`{n,m}`**：重复n到m次
	- **`{n,}`**：重复至少n次
- **转义字符:** `\\.`字面上的点， `\\*` 字面上的星号， `\\(\\)` 字面上的括号
- **锚点:** **`^`**：字符串开头 **`$`**：字符串结尾 **`\b`**：单词边界
- **通配符** **`.`**：匹配任意单个字符 **`.*`**：匹配任意字符，重复任意次数 **`\\s`:** 任何空白字符
- **前瞻与后顾:**
	- **正向前瞻** `(?=pattern)`: 后面必须跟着pattern
	- **负向前瞻** `(?!pattern)`: 后面不能跟着pattern
	- **正向后顾** `(?<=pattern)`: 前面必须是pattern
	- **负向后顾** `(?<!pattern)`: 前面不能是pattern
- **捕获组:** `(?<name>pattern)` 捕获匹配的内容并命名为 name
- **非捕获组:** `(?:<text>)` 仅用于分组，不捕获
- **regexpr(pattern, text):** 在每个字符串中查找**第一个**匹配的位置和长度
- **regexec(pattern, text, perl = TRUE):** 正则表达式捕获
- **regmatches(x, m):** 根据 `regexpr()` 的结果提取匹配的子字符串
	* **x:** 原始字符串向量
	* **m:** regexpr()返回的匹配信息（位置+长度）
* **regmatches(x, m) 赋值功能：** 将匹配的部分替换为新字符串
	* **x:** 要修改的原始向量
	* **m:** 匹配信息（位置+长度）
- **gregexec(pattern, text, perl = TRUE, ignore.case = FALSE):
	* **perl = TRUE:** 使用 Perl 兼容正则（否则负向后顾断言以及命名捕获组会报错）
	* **ignore.case = FALSE:** 忽略大小写
	* 和 **gregexpr()** 的区别: 
		* gregexec() 返回一个**矩阵**
		* gregexpr() 返回一个**向量**
* **grep():**
	* 筛选列名:
		* grep("speed$", names(dt))  -----返回位置索引 
		* grep("speed$", names(dt), value = TRUE)  -----返回列名本身
	* 筛选符合条件的行:
		* dt[grep("Scout", class)]  -----找出名字包含 "Scout" 的行
	* 从向量中筛选元素:
		* fruits <- c("apple", "banana", "apricot", "cherry")
		* grep("^a", fruits)  -----返回 1, 3
	*  **grepl():** 逻辑向量 (TRUE/FALSE)
		* grepl("^a", fruits)  -----返回 c(TRUE, FALSE, TRUE, FALSE)
	* 和 `regmatches() + regexpr()` 的区别:
		* grep(..., value=TRUE) 匹配的**整个元素**
		* regmatches(regexpr()) 匹配的**具体部分**
* **gsub(pattern, replacement, x):** 替换所有匹配
* **sub(pattern, replacement, x):** 只替换第一个匹配
* **setnafill(x, fill):** 用于填充NA值，返回修改后的对象，但是不可见
	* fill = 0: 把所有 **NA** 值替换为 0


## 向量操作 
* **head(x, n):** 取前 n 个元素 
* **tail(x, n):** 取后 n 个元素 
* **rev(x):** 反转向量 
* **order(...):** 返回排序后的索引 
	* 示例: order(c(3,1,2)) → c(2,3,1) 
	* `x[order(x)]` 等价于 `sort(x)`
* **rank(x):** 返回每个元素的排名 
* **diff(x):** 计算相邻元素的差


## 列表
* **rbindlist():**
	* use.names = TRUE: 按列名匹配（默认值）
		* use.names = FALSE: 按位置匹配
	* fill = TRUE: 缺失列用 **NA** 填充


## 矩阵
* **`mat[ , , drop = FALSE]`:** drop = FALSE 确保返回一个矩阵，否则是一个向量 (**注意有两个逗号!!!**)
* **`mat[numeric(0),nrow = 0, ncol = 0]`:** 生成 numeric(0) 矩阵
* **length(mat) == 0:** 返回**元素总数**（行数 × 列数）为0的情况，处理空矩阵用
* **row(mat) / col(mat)**: 获取行 / 列索引
* **rowSums():** 行和
* **colSums():** 列和
* **cbind():** 按**列**合并（左右拼接）
	* `mat[cbind(i, j)]` <- value: 一次性给多个位置赋值，批量索引矩阵
* **rbind():** 按**行**合并（上下拼接）
* **arrayInd():** 将线性索引转换为多维数组的坐标
* **diag():** 提取或创建对角矩阵
	* 用法一：**diag(matrix)** 提取主对角线元素
	* 用法二：**diag(vector)** 用向量创建对角矩阵
	* 上对角线: 第i行第i-1列 
		*  `for (i in 2:x$n) { mat[i, i - 1] <- x$lower[i - 1]}`(n是矩阵维度，lower是下对角线元素)
	* 下对角线: 第i行第i+1列
		* `for (i in 1:(x$n - 1)) { mat[i, i + 1] <- x$upper[i]}`(n是矩阵维度，upper是上对角线元素)
* **t():** 转置
* **replicate(n, expr):** 执行表达式 n 次，将结果组合成矩阵或数组
* **apply(X, MARGIN, FUN, ...):**
	* 参数：
		* **X:** 矩阵
		* **MARGIN:** 1 表示按行操作，2 表示按列操作
		* **FUN:** 对每一行/列应用的函数
		* **...:** 传递给 **FUN** 的参数
	* 工作原理：
		* 遍历矩阵的每一行/列
		* 将每一行/列作为向量传递给后面的函数
		* 收集所有行/列的结果并返回一个向量
	* **例子:** 保留矩阵中不全部为 **NA** 的行
		* `keep <- apply(mat, 1, function(row)) {!all(is.na(row))} --- mat[keep, , drop = FALSE]`


## 数据框
* **`df[ , , drop = TRUE]`:** 得到一个向量，丢失了列名，也丢失了 data.frame 的属性
	* **drop = FALSE**: 无论剩下几列（哪怕只有 1 列），它依然保持为一个 `data.frame`
* **nrow():** 行数
* **ncol():** 列数
* **rowSums():** 行和
* **colSums():** 列和
* **cbind(i, j):** 横向，列合并
* **rbind():** 纵向，行合并


## DataTable
* **注意：** datatable只有列名，行名只有1, 2, 3, ...
* **setDT():** 转换为 data.table
* **rbindlist():** 将小列表纵向堆叠，生成最终的 DataTable
* 用 **.()** 可以命名多列
* `:=` :引用修改
	* 创建新列：`dt[, new_col := value]`
	* 删除列：`dt[, col := NULL]`
	* 修改多列：`dt[, `:=` (col1 = val1, col2 = val2)]`(反引号包裹`:=`)
* **`=`**: 传递参数
* **.SD**: 当没有指定 `.SDcols` 时，`.SD` 包含**除分组列以外的所有列**
	* 取特定行`.SD[n]`
	* 操作多列`lapply(.SD, func)`
	* 筛选子表`.SD[condition]`
* **.SDcols = patterns("^sensor")**: 正则表达式选择特定列
* **cbind():** 横向，列合并
* **rbind():** 纵向，行合并
* **升序降序:**
	* `dt[order(-列名)]`: 降序
	* `dt[order(列名)]`: 升序
* **setorderv(dt, cols, order = 1):** 按指定列排序
	* `c("missings", "quality")`: 先按 missings 排序，再按 quality 排序
	* `order = -1`: 降序排列
		* 1 或 c(1, 1): 全部升序
		* -1 或 c(-1, -1): 全部降序
		* c(1, -1): 第一列升序，第二列降序
* **merge():** 合并与连接
	* `merge(x, y, by, all, all.x, all.y, ...)`
		* all: 全外连接（保留两表所有行）
		* all.x: 左连接（保留左表所有行）
		* all.y: 右连接（保留右表所有行）
	* **左连接**(不用merge): `y[x, on = "..."]` **=>** 保留 **x** 的所有行
		* 多个连接键时 `on = .(a, b)` 或 `on = c("a", "b")`
		* 笛卡尔积(**多对多自连接**): `y[x, on = "...", allow.cartesian = TRUE]`
			* 两个表有 **同名列** 时: 左表(x)列名无前缀，右表(y)列名有 `i.` 前缀
	* **反向连接:** 返回只在 x 表中但不在 y 表里的行
		* `x[!y, on = "..."]`
	* **滚动连接:** `x[!y, on = "...", roll = TRUE]`
		* **roll** 参数值:
			* roll = TRUE / Inf -----向前滚动（找 ≤ 的最近值）
			* roll = -Inf -----向后滚动（找 ≥ 的最近值）
			* roll = n -----最多滚动 n 个单位
			* roll = "nearest" -----找最接近的（前或后）
* **分组聚合:**
	* `data[, .(新列 = 聚合函数(列)), by = 分组列]`
		* **需要手动排序** --- `data[, .(), by][order()]`
		* 根据多列分组: by = .(..., ...)
* **切片:**
	* 取前n行：`dt[1:n]`
	* 取后n行：`tail(dt, n)`
	* 取特定位置的行：`dt[c(a, b, c)]`
	* 取最大值的行：`dt[order(-quantity)][1:n]`
	* 取最小值的行：`dt[order(quantity)][1:n]`
	* 按比例取样(z.B. 取前10%)：`dt[1:(.N * 0.1)]`
* **去重:**
	* 对指定列去重，保留所有列：`unique(dt, by = c("store.loc", "product.id"))`
	* 对指定列去重，只保留这些列：`unique(dt[, .(store.loc, product.id)])`
* **last():** 取最后一个元素
* **copy():** 创建外部传入的 data.table 的**深拷贝**，修改副本不会影响原数据
* **setnafill(dt, fill = 0, cols = "列名"):** 原地修改，填充 **NA** 值 (只能填充**数值列**)
	* 场景: 多列、直接修改、追求效率 (只在**datatable**)
* **nafill():** 返回新向量，填充 **NA** 值，需要赋值
	* 场景: 单列、需要保留原数据 (可以在**向量**和 **datatable**)
* **dcast(data, 行变量 ~ 列变量, value.var = "填充值"):** 长格式转宽格式 (dataframe中也可使用)
	* 行变量: 各一行
	* 列变量: 各一列，展开为列
	* 填充值: 填入对应格子的值
	* **注意:** 使用之后可能会打乱行顺序，需要用原始变量顺序做连接，恢复顺序 (`[...$..., on = "...."]`)


## 参数验证
##### 数值类型
* **assertNumeric()**: 数值型**向量**
* **assertNumber()**: 数值型**标量**
	* 正整数: lower = 1; 负整数: upper = -1
* **assertCount(x, positive = FALSE):** **单个非负整数**
	* positive = TRUE 时不允许 0
* **assertIntegerish():** **严格整数型向量**
* **assertInt(x, , tol = 1e-100):** **单个整数** (允许 1.0 这种整数值)
	* tol = 1e - 100: 严格整数，容差极小，可加可不加
##### 字符类型
* **assertCharacter():** 字符向量
* **assertString():** 单个(标量)字符串

##### 逻辑类型
* **assertLogical(x):** 逻辑向量
* **assertFlag():** **单个逻辑值**
* **assertTRUE(x) / assertFALSE(x):** 严格 TRUE/FALSE

##### 复杂结构
* **assertList(x, names = ...):
	* names = "unnames": 无名称，列表没有名称时通过检查 (x <- list(1, 2, 3))
	* names = "names": 有名称，列表所有元素都有名称时通过检查 (x <- list(a = 1, b = 2))
		* names = **"unique"**: 必须有名称且不重复
* **assertMatrix(mat, mode)**: 检查是否为**矩阵**
* **assertDataFrame()**: 检查是否为**数据框**
* **assertDataTable()**: 检查是否为 **DataTable**
* **assertFunction(X, nargs):** 检查是否为**函数**类型
	* **nargs:** 参数数量

##### 集合与选择
* **assertSubset(a, b):** 检查 a 是否为 b 的子集
* **assertChoice(a, b)**: 检查 a 是否在 b 中
* **assertNames(names(...), permutation.of = c("...", "...")):** 检查名字
	* 等价于: assertSubset(c("...", "..."), names(...))

##### 日期时间
* **assertPOSIXct():** **验证对象是否为 POSIXct 类型**（日期时间格式）
* **assertDate(x):** Date 类型

##### 其他
* **assertNull(x):** NULL
* **assertClass(x, classes):** 指定类
* **assertNamed(x):** 有名称


## 变量
* 使用 **<<-** 修改**全局变量**，使用 **<-** 会创建**局部变量**


## 日期
* **ISOdate():** 将年、月、日转换为日期对象，便于比较
	* ISOdate(2008, 12, 8) **=>** "2008-12-08 12:00:00 GMT"


## S3Class
* **模版:**
```r
# ============================================
# 构造函数
# ============================================
ClassName <- function(arg1, arg2, arg3) {
  
  # 1. 参数检查
  
  # 2. 自定义检查
  if (arg2 > arg1) {
    stop("自定义错误信息")
  }
  
  # 3. 创建对象
  structure(
    list(
      field1 = arg1,
      field2 = arg2,
      field3 = arg3
    ),
    class = "ClassName"
  )
}

# print 方法
print.ClassName <- function(x, ...) {
  # 格式化输出
  cat(sprintf("A ClassName object with %d and %s.\n",
    x$field1, x$field2
  ))
  # 返回对象（不可见）
  invisible(x)
}

# dim 方法（如果是矩阵类）
dim.ClassName <- function(x) {
  c(x$nrow, x$ncol)
}

# as.matrix 方法（如果需要转换）
as.matrix.ClassName <- function(x, ...) {
  # 创建矩阵
  mat <- matrix(0, nrow = x$nrow, ncol = x$ncol)
  # 填充值
  mat[x$i, x$j] <- x$v
  mat
}
```


## R6Class
**基本结构:**
```r
library(R6)

ClassName <- R6Class("ClassName",
  # 私有成员（外部无法直接访问）
  private = list(
    data = NULL,
    helper_function = function() {
      # 私有方法
    }
  ),
  
  # 公有成员（外部可以访问）
  public = list(
    # 初始化方法
    initialize = function(arg1, arg2 = default_value) {
      # 1. 输入验证（使用 checkmate 包）
      assertNumeric(arg1, lower = 0, any.missing = FALSE)
      
      # 2. 初始化私有/公有变量
      private$data <- arg1
    },
    
    # 返回 invisible(self) 的方法（支持链式调用）
    chainable_method = function(item) {
      # 输入验证
      assertString(item)
      
      # 错误处理
      if (condition) stop("error message")
      
      # 修改状态
      private$data <- new_value
      
      # 返回 invisible(self) 以支持链式调用
      invisible(self)
    },
    
    # 普通 getter 方法
    getData = function() {
      private$data
    }
  )
)
```

**基本继承结构:**
```r
# 父类
ParentClass <- R6Class("ParentClass",
  private = list(
    name = NULL
  ),
  public = list(
    initialize = function(name) {
	  assertString(name)
      private$name <- name
    },
    getName = function() {
      private$name
    },
    describe = function() {
      paste("I am", private$name)
    }
  )
)

# 子类
ChildClass <- R6Class("ChildClass",
  inherit = ParentClass,  # 关键：指定父类
  
  private = list(
    age = NULL  # 子类新增的私有变量
  ),
  
  public = list(
    # 重写初始化方法
    initialize = function(name, age) {
      super$initialize(name)  # 调用父类初始化
      private$age <- age
    },
    
    # 子类新增方法
    getAge = function() {
      private$age
    },
    
    # 重写父类方法
    describe = function() {
      paste(super$describe(), "and I am", private$age, "years old")
    }
  )
)
```