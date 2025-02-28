# Use SWI-Prolog as the base image
FROM swipl:latest

# Create a work directory
WORKDIR /app

# Copy your Prolog files into the container; changed from "COPY src/*.pl ./"
COPY src/ ./

# Create a non-privileged user
ARG UID=10001
RUN adduser \
    --disabled-password \
    --gecos "" \
    --home "/nonexistent" \
    --shell "/sbin/nologin" \
    --no-create-home \
    --uid "${UID}" \
    appuser

# Set ownership of the application files
RUN chown -R appuser:appuser /app

# Switch to non-privileged user
USER appuser

# List the contents of the /app directory for debugging; changed from "RUN ls"
RUN ls -la /app

# Command to run your main Prolog file
ENTRYPOINT ["swipl", "-s", "./server.pl"]