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

                if (env.BRANCH_NAME.equals('production')) {
                    stage ('Build docker image') {
                        sh """
                            echo "Build docker image"
                        """
                    }

                    stage ('Push image to DockerHub') {
                        sh """
                            echo "Push image to DockerHub"
                        """
                    }

                    stage ('Deploy to production') {
                        sh """
                            echo "Deploy to production"
                        """
                    }

                    stage ('Verify health checks') {
                        sh """
                            echo "Verify health checks"
                        """
                    }
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

