# Installation tips


## Formatter for IntelliJ IDEA

1. Copy intellij-java-google-style.xml into your config/codestyles folder in your IntelliJ settings folder. 

2. Under Settings/Code Style select the "GoogleStyle" as current code style for the project.

**Tip** You can create a symbolic link from formatter file in jvm-bloggers project directory to the confif/codestyles directory. Still you need to restart IDEA to see the changes.

## Formatter for Eclipse

1. Under Window/Preferences select Java/Code Style/Formatter. Import the settings file by selecting Import.

## Formatter for NetBeans

Unfortunately there is no formatter for NetBeans with Google Java Style. You can try to use [Eclipse Java Code Formatter Integration for NetBeans](http://plugins.netbeans.org/plugin/50877/eclipse-code-formatter-for-java).