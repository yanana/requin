#!/usr/bin/env bash

set_output() {
  local name="$1"
  local value="$2"

  value="${value//'%'/'%25'}"
  value="${value//$'\n'/'%0A'}"
  value="${value//$'\r'/'%0D'}" 

  echo "::set-output name=${name}::${value}"
}

