# Ia que fez a maior parte, o resto peguei das outras atividades, não sei mexer com docker

# Use an official Haskell image as a base image
FROM haskell:latest

# Install any necessary tools
RUN apt-get update && apt-get install -y \
    build-essential \
    curl
    
# Instalar PostgreSQL
RUN apt-get update && apt-get install -y \
    wget \
    ca-certificates \
    postgresql postgresql-contrib

# Limpar cache do apt
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Configurar o PostgreSQL para rodar automaticamente
RUN service postgresql start

# Set up the working directory
WORKDIR /workspace