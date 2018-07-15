#!/usr/bin/env groovy

properties(
    [
        pipelineTriggers([
                [$class: 'GitHubPushTrigger']
        ]),
        overrideIndexTriggers(false),
        disableConcurrentBuilds(),
        buildDiscarder(logRotator(artifactDaysToKeepStr: '', artifactNumToKeepStr: '', daysToKeepStr: '', numToKeepStr: '10'))
    ]
)

node {
    try {
        timestamps {
            ansiColor('xterm') {
                stage ('Checkout') {
                    checkout scm
                }

                stage ('Test') {
                    sh """
                        ./gradlew clean build
                    """
                }
            }
        }
    } catch (Exception e) {
        currentBuild.result = "FAILED"
        throw e
    } finally {
        junit '**/build/test-results/**/TEST-*.xml'
    }

}

