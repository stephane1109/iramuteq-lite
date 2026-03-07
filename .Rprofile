# Startup options for container runtimes (e.g. Hugging Face Spaces)
# Apply BSPM option as early as possible (before app code) to avoid D-Bus warning noise.
options(bspm.sudo = TRUE)

# Some managed environments expose a user-level bspm setup without D-Bus service.
# Disabling it prevents fragile startup behavior in non-systemd containers.
Sys.setenv(R_BSPM_DISABLE = "true")
