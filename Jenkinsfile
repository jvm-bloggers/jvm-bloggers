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
                        echo "Building the project"
                        ./gradlew clean build
                    """
                }

                if (env.BRANCH_NAME.equals('production')) {
                    stage ('Build Docker image') {
                        sh """
                            echo "Build Docker image"
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

        if (currentBuild.currentResult == "SUCCESS") {
            sh """
                echo "Notyfying Codecov after successful build"
                bash <(curl -s https://codecov.io/bash)
            """
            sh """
                echo "Notyfying Sputnik after successful build"
                python <(curl -s https://raw.githubusercontent.com/TouK/sputnik-ci/master/sputnik-ci.py)
            """
        }
    }

}

