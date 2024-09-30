# Regression Spanish

Online version is available at https://openacttextdev.github.io/RegressionSpanish/index.html

Here are the instructions given to ChapGPT for the translation:

>Hello. We are in the process of translating an undergraduate textbook on actuarial science from english to spanish. The following text is an english version, written in R's markdown language. Can you provide similar text in markdown but in spanish? Please use simple language, when possible. Variables are words in all upper case, so please do not translate these. Expressions within mathjax are mathematical expression, so please do not translate these.

Here are the instructions given to ChapGPT for the conversion from latex to R markdown.

>For consistency, please use the basic version of ChatGPT. The following is part of latex file. Please convert this into R markdown. Please use `$` for inline equations and `$$` for other equations, consistent with MathJax format. Use MathJax for rendering the mathematical expression. This is conversion ("translation") only - no additional interpretations. Variables are words in all upper case, so please do not change these nor use inline equation format for them.

>Please convert the following into R markdown. Use MathJax for rendering the mathematical expressions - use $ for inline equations and $$ for other equations, consistent with MathJax format. Do not use \[ formats....  This is conversion only - no additional interpretations and retained the English. Some words are in all upper case, please leave them as is - DO NOT enclose using '$' for inline equations. Please use markdown symbols for italics and bold, not \textit and \textbf.


(ref:Extrapolation) **Extrapolation outside of the sampling region may be biased** 

```{r Extrapolation, fig.cap='(ref:Extrapolation)', echo=FALSE, fig.align='center', out.width = "80%"}
#Term <- read.csv("CSVData/TermLife.csv", header=TRUE)

```