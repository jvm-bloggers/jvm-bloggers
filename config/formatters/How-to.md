# IDE configuration tips


## IntelliJ IDEA formatter

1. Copy [`intellij-java-google-style.xml`](https://github.com/tdziurko/jvm-bloggers/blob/master/config/formatters/intellij-java-google-style.xml) into your `config/codestyles` folder in your _IntelliJ IDEA_ settings folder. 

2. Under `Settings > Code Style` select the _GoogleStyle_ as current code style for the project.

**Tip:** You can create a symbolic link to formatter file in _jvm-bloggers_ project directory from the `config/codestyles` directory. Still you need to restart _IDEA_ to see the changes.

## Eclipse formatter

1. Under `File > Import` select `Preferences` and press `Next >` button. Then form _Import Preferences_ dialog `Browse` for the [`eclipse-java-google-style.epf`](https://github.com/tdziurko/jvm-bloggers/blob/master/config/formatters/eclipse-java-google-style.epf) file and import it by pressing `Finish`.

## NetBeans formatter
Unfortunately there is no formatter for _NetBeans_ with Google Java Style. You can try to use [Eclipse Java Code Formatter Integration for NetBeans](http://plugins.netbeans.org/plugin/50877/eclipse-code-formatter-for-java).
