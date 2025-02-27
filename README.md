# Amalzen-Disease-Diagnosis
[![HTML5](https://img.shields.io/badge/HTML5-E34F26?style=for-the-badge&logo=html5&logoColor=white)](https://developer.mozilla.org/en-US/docs/Web/HTML)
[![CSS3](https://img.shields.io/badge/CSS3-1572B6?style=for-the-badge&logo=css3&logoColor=white)](https://developer.mozilla.org/en-US/docs/Web/CSS)
[![JavaScript](https://img.shields.io/badge/JavaScript-F7DF1E?style=for-the-badge&logo=javascript&logoColor=black)](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
[![Docker](https://img.shields.io/badge/Docker-2496ED?style=for-the-badge&logo=docker&logoColor=white)](https://www.docker.com/)
[![Docker Compose](https://img.shields.io/badge/Docker%20Compose-2496ED?style=for-the-badge&logo=docker&logoColor=white)](https://docs.docker.com/compose/)
[![Nginx](https://img.shields.io/badge/Nginx-009639?style=for-the-badge&logo=nginx&logoColor=white)](https://nginx.org/)
[![Git](https://img.shields.io/badge/Git-F05032?style=for-the-badge&logo=git&logoColor=white)](https://git-scm.com/)
[![GitHub](https://img.shields.io/badge/GitHub-181717?style=for-the-badge&logo=github&logoColor=white)](https://github.com/)

[![Prolog](https://img.shields.io/badge/Prolog-SWI-orange.svg)](https://www.swi-prolog.org/)
[![Docker](https://img.shields.io/badge/Docker-Ready-blue.svg)](https://www.docker.com/)
[![Version](https://img.shields.io/badge/version-1.0.0-green.svg)](https://github.com/yourusername/Amalzen-Disease-Diagnosis)

Amalzen Disease Diagnosis is a program for identifying a user's disease based on symptoms using the Prolog Programming Language.

## Setting Up your environment

1. **Install Docker and Docker Compose**: Ensure you have Docker and Docker Compose installed on your machine. You can download Docker Desktop from [Docker's official website](https://www.docker.com/products/docker-desktop).

2. **Clone the Repository**:
    ```sh
    git clone https://github.com/yourusername/Amalzen-Disease-Diagnosis.git
    cd Amalzen-Disease-Diagnosis
    ```
3. **Install SWI-Prolog(Optional):** Install SWI-Prolog if you want to use Prolog in your machine. You can download it from [SWI-Prolog's official website](https://www.swi-prolog.org/Download.html).

## Running with Docker

### Building and Running Containers

1. **Build all containers** using Docker Compose:
    ```sh
    docker-compose up --build
    ```
   This will build and start all services (prolog-api, frontend, and nginx).

2. **Run all containers**
    ```sh
    docker-compose up
    ```
    This will run all services

   To Run the frontend using nodemon use the command (for Windows):
   ```sh
   $env:NODE_ENV="development"; docker-compose up --build
   ```
   Or macOS/Linux
   ```sh
   NODE_ENV=development docker compose up --build
   ```

4. **Stop all containers**
    ```sh
    docker-compose down
    ```
    This will stopp all services

3. **Access the application**:
   - 
   - Web interface: http://localhost:80 (served by nginx)
   - Direct API access: http://localhost:8090/diagnose?symptoms=fever,cough

   ![Disease Diagnosis Interface](docs/image.png)

### Running via CLI

1. Navigate to the src directory:
    ```sh
    cd backend/src
    ```

2. Start the Prolog interpreter:
    ```sh
    swipl
    ```

3. Run the main module:
    ```prolog
    ?- [main].
    ```

4. Follow the on-screen instructions to use the interactive mode or start the web server.


## Project Structure

- backend: Contains the Prolog backend code
  - `src/knowledge.pl`: Knowledge base with diseases and their symptoms
  - `src/logic.pl`: Diagnosis logic
  - `src/server.pl`: Web server code
  - `src/main.pl`: CLI entry point
- frontend: Contains the web interface
- `docker-compose.yaml`: Docker Compose configuration
- nginx.conf: Nginx configuration

