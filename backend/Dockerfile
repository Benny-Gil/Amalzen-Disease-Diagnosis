# Use SWI-Prolog as the base image
FROM swipl:latest

# Create a work directory
WORKDIR /app

# Copy your Prolog files into the container
COPY src/*.pl ./

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

RUN ls

# Command to run your main Prolog file
# Replace 'main.pl' with your main Prolog file name
ENTRYPOINT ["swipl", "-s", "./server.pl"]

# If you want to run it in interactive mode, use this instead:
# ENTRYPOINT ["swipl"]