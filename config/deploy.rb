# config valid only for current version of Capistrano
lock '3.3.5'

set :application, "reedink"
set :repo_url, "git@github.com:darthdeus/reedink.git"

# Default deploy_to directory is /var/www/my_app_name
set :deploy_to, "/opt/apps/reedink"

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
set :linked_files, fetch(:linked_files, []).push("config/settings.yml")

# Default value for linked_dirs is []
# set :linked_dirs, fetch(:linked_dirs, []).push('bin', 'log', 'tmp/pids', 'tmp/cache', 'tmp/sockets', 'vendor/bundle', 'public/system')
set :linked_dirs, fetch(:linked_dirs, []).push('tmp/pids')

# Default value for default_env is {}
set :default_env, { path: "/opt/ghc/7.8.4/bin:$HOME/.cabal/bin:$PATH" }

namespace :deploy do

  task :restart do
    on roles(:web), in: :sequence, wait: 10 do
      within release_path do
        execute :sudo, :restart, :reedink
      end
    end
  end

  after :updated, :init_sandbox do
    on roles(:app), in: :sequence, wait: 5 do
      within release_path do
        execute :cabal, "sandbox", "init", "--sandbox", "/opt/apps/.reedink-sandbox"
      end
    end
  end

  after :init_sandbox, :cabal_build do
    on roles(:app), in: :sequence, wait: 5 do
      within release_path do
        execute :cabal, "configure"
        execute :cabal, "build"
      end
    end
  end


end
