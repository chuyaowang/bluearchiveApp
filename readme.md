# 这是一个计算爱丽丝暴击的APP

## 功能

- 点击按键累计暴击数
- 根据结果统计暴击数量，暴击率，计算消耗时间
- 输出暴击数柱状图，暴击率变化图
- 暴击统计结果可以下载
- 当然也可以用于其他需要计数并简单统计的场景

## 安装

- 基于R Shiny，目前需要用R Studio打开运行
- 首先安装R和R Studio
- 然后Clone仓库并安装需要的包即可使用
- 正在学习怎么转换为网页应用


## 后记

- 可能会出现图中的中文字符不能显示的问题，使用`showtext`包可以解决
    ```r
    library(showtext)
    font_add("Songti", regular = "ttc格式的字体文件路径")
    showtext_auto()
    ```
- 纯发癫作品，未来可能添加更多的功能