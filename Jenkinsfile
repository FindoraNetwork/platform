pipeline {

  environment {
    dockerRepo = 'https://nexus.findora.org'
    dockerCreds = 'nexus'
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
            dockerImage = docker.build( dockerName + ":" + env.BRANCH_NAME, '--pull .')
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

    stage('Push') {
      when {
        branch 'master'
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
