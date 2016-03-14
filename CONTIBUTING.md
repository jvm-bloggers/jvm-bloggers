# Contributing


## If looking for help

* Please use our [Gitter chat room](https://gitter.im/tdziurko/jvm-bloggers) full of helpful people :)

* Not sure which issue pick? Please check those labelled as ["For newcomers"](https://github.com/tdziurko/jvm-bloggers/issues?q=is%3Aissue+is%3Aopen+label%3A%22For+newcomers%22)


## Code formatting

* Please use code formatters for your IDE (at this moment IntelliJ and Eclipse are supported). Files to import in your IDE are located in `/formatters` dir

* Avoid reformatting code that is not affected by your changes, this will make diff of your PR much more concise


## Effective cooperation tips

* Before coding please comment on issue that you will be working on it.

* Before coding think if you know everything you need and ask for a clarification when you are not sure about something (especially about "functional" requirements)
 
 

## General coding guidelines

* Avoid Javadocs where possible. Comments should be used only when REALLY needed, prefer better naming and clean code over bloated Javadocs.

* `@author` tag shouldn't be placed in test classes unless class is a utility/helper class and not a pure test class

* `@since` is not needed as we do not have public API

* Always provide a test to your change

 
## Testing
  
* We use [Spock](http://spockframework.github.io/spock/docs/1.0/index.html) as a base testing framework

* Notation for test classes is `YourClassNameSpec` ([example](https://github.com/tdziurko/jvm-bloggers/blob/master/src/test/groovy/pl/tomaszdziurko/jvm_bloggers/utils/DateTimeUtilitiesSpec.groovy#L8))

* Notation for test methods is `def "Should do something"` ([example](https://github.com/tdziurko/jvm-bloggers/blob/master/src/test/groovy/pl/tomaszdziurko/jvm_bloggers/InitialBlogDataPopulationTriggerSpec.groovy#L20))

* Each section after label `given/when/then` should have indentation one level deeper than label itself
  
* For consistency we use static typing in favour of `def`


## Commit guidelines

* Do not forget to run `./gradlew check` before the commit. Green unit tests under IDE (or `./gradlew test`) does not guarantee the commit will pass CI. `check` task will additionally (among unit tests) run [checkstyle](http://checkstyle.sourceforge.net/) plugin against source code base.

* Try to add issue number in each commit message `Adds new entity class and liquibase script, #123`

* One (preferably last or final) commit should have `fixes #123` or `closes #123` so after merging Pull Request corresponding issue will be closed by GitHub (see more datails in [this article](https://github.com/blog/1386-closing-issues-via-commit-messages))


## Reviewing and merging

* If code review has been done and the PR has been successfully merged then [delete the PR branch](https://help.github.com/articles/deleting-unused-branches/) using _Delete branch_ button at the bottom of given PR page.
