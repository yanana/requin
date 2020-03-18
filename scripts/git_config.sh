#!/usr/bin/env bash

git_config() {
  local committer_name='GitHub'
  local committer_email='noreply@github.com'

  git config user.name "${committer_name}"
  git config user.email "${committer_email}"
}
