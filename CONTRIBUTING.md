# Contributing


## If looking for help

* Please use our [Gitter chat room](https://gitter.im/tdziurko/jvm-bloggers) full of helpful people :)

* Not sure which issue pick? Please check those labelled as ["For newcomers"](https://github.com/tdziurko/jvm-bloggers/issues?q=is%3Aissue+is%3Aopen+label%3A%22For+newcomers%22).

## Local git configuration

Contributors must configure local repository after cloning by executing the command from the cloned root directory:

	git config --local include.path ../.gitconfig

This will configure cloned repository according to project standards. Note that it will not alter your global or system git configuration, it will only modify `.git/config` file located in the cloned root directory. This is required to run checkstyle task properly (required by [commit guidelines](#commit-guidelines)).

## Code formatting

* Please use code formatters for your IDE (at this moment IntelliJ and Eclipse are supported). Files to import to your IDE are located in [`/formatters`](config/formatters) directory. See [IDE Configuration](config/formatters/How-to.md) tips for details.

* Avoid reformatting code that is not affected by your changes, this will make diff of your PR much more concise


## Effective cooperation tips

* Before coding please comment on issue that you will be working on it.

* Tag the issue with the yellow `In progress` label if you start working on it ([collaborators](https://help.github.com/articles/permission-levels-for-a-user-account-repository/#collaborator-access-on-a-repository-owned-by-a-user-account) only).

* Before coding think if you know everything you need and ask for a clarification when you are not sure about something (especially about "functional" requirements)
 
 

## General coding guidelines

* Avoid Javadocs where possible. Comments should be used only when REALLY needed, prefer better naming and clean code over bloated Javadocs.

* `@author` tag shouldn't be placed in test classes unless class is a utility/helper class and not a pure test class

* `@since` is not needed as we do not have public API

* Always provide a test to your change

* Avoid one-time variable. If you decide to use it, be prepared to explain the reason behind 


 
## Testing
  
* We use [Spock](http://spockframework.github.io/spock/docs/1.0/index.html) as a base testing framework

* Notation for test classes is `YourClassNameSpec` ([example](src/test/groovy/pl/tomaszdziurko/jvm_bloggers/utils/DateTimeUtilitiesSpec.groovy#L8))

* Notation for test methods is `def "Should do something"` ([example](src/test/groovy/pl/tomaszdziurko/jvm_bloggers/InitialBlogDataPopulationTriggerSpec.groovy#L20))

* Each section after label `given/when/then` should have indentation one level deeper than label itself
  
* Do **not** use `def` for local variables declaration in `.groovy` files, we use static typing instead


## Commit guidelines

* Do not forget to run `./gradlew check` before the commit. Green unit tests under IDE (or `./gradlew test`) does not guarantee the commit will pass CI. `check` task will additionally (among unit tests) run [checkstyle](http://checkstyle.sourceforge.net/) plugin against source code base.

* Try to add issue number in each commit message `Adds new entity class and liquibase script, #123`

* One (preferably last or final) commit should have `fixes #123` or `closes #123` so after merging Pull Request corresponding issue will be closed by GitHub (see more datails in [this article](https://github.com/blog/1386-closing-issues-via-commit-messages))


## Reviewing and merging

* If code review has been done and the PR has been successfully merged then [delete the PR branch](https://help.github.com/articles/deleting-unused-branches/) using _Delete branch_ button at the bottom of given PR page.

* After successfull merge the issue given PR referes to should be automatically closed (if you conformed to [commit guidelines](#commit-guidelines) ealier). If it was not closed automatically by merge commit then close the related issue manually.

* Finally remove the yellow `in progress` label from the closed issue ([collaborators](https://help.github.com/articles/permission-levels-for-a-user-account-repository/#collaborator-access-on-a-repository-owned-by-a-user-account) only).
