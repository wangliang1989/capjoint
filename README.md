# CAPjoint

CAPjoint 是中科大的 Weiwen Chen 和 Zhe Jia 基于朱露培教授的 CAP 发展而来，其优势在于可以利用远震数据。
CAPjoint 的官方发布地址是 http://home.ustc.edu.cn/~vincentc/CAPjoint

这个 repo 是我在学习 CAPjoint 后整理并用 perl 改写其主要脚本后的脚本集。对 CAPjoint 方法本身并无任何改进。
如果按照下面的流程，原则上可以把官方包内的例子跑出来。详细的笔记请看我的[博客](http://blog.wangliang.one/archives/2017-04-20_capjoint/)

## 编译与安装

地震学科研一般都要用的工具在此处不指导如何安装，只列出。此处的链接是指向安装帮助，这些软件都是必须的，请勿跳过：
1. [SAC](https://seisman.github.io/SAC_Docs_zh/)
2. [Tau-P](http://seisman.info/install-taup.html)
3. [fk](http://seisman.info/fk-installation.html)
4. [GMT4](http://seisman.info/install-gmt4-under-linux.html)
5. [PSSAC2](http://seisman.info/install-pssac2.html)

### 下载

````
git clone git@github.com:wangliang1989/capjoint.git
````

### 编译 crust2

进入 `src/crust2`,编译：

````
gfortran getCN2point.f -o getCN2point
````

### 编译 teles

进入 `src/tel3`,编译：

````
make
````

### 编译 CAP

进入 `src/cap`,编译：

````
make
````

**在编译完 teles 和 cap 之后，会分别生成可执行文件 teles 和 cap，请务必把他们拷贝到 `example/cmds` 下**


## 运行例子

例子放在 `example` 路径下。

### 数据预处理

````
perl process.pl 20140310
````
### 计算格林函数

````
perl grn.pl 20140310
````
### cap 反演

````
perl inversion.pl 20140310
````
