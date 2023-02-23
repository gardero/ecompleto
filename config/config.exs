
# General application configuration
import Config

# Configures Elixir's Logger
config :logger, :console,
       format: "$time $metadata[$level] $message\n",
       level: :info,
       metadata: [:request_id]