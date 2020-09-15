pipeline {

  environment {
    dockerRepo = 'https://563536162678.dkr.ecr.us-west-2.amazonaws.com'
    dockerCreds = 'ecr:us-west-2:aws-jenkins'
    oldDockerRepo = 'https://nexus.findora.org'
    oldDockerCreds = 'nexus'
    dockerName = 'platform'
  }

  agent any

  options {
    ansiColor('xterm')
  }

  stages {

    stage('Prep') {
      steps {
        script {
          sh 'bash ./scripts/docker_prep.sh'
        }
      }
    }

    stage('Build') {
      steps {
        script {
          docker.withRegistry( dockerRepo, dockerCreds ) {
            dockerImage = docker.build( dockerName + ":" + env.BRANCH_NAME, '--network redis --pull .')
          }
        }
      }
    }

    stage('Extract/Archive .deb') {
      steps {
        script {
          dockerImage.inside() {
            sh 'cp /app/debian/*.deb $WORKSPACE'
          }
          archiveArtifacts artifacts: '*.deb'
        }
      }
    }

    stage('Push ECR') {
      when {
        not {
          changeRequest()
        }
      }
      steps {
        script {
          docker.withRegistry( dockerRepo, dockerCreds ) {
            dockerImage.push()
          }
        }
      }
    }

  }

}
