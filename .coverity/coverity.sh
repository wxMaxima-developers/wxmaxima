#!/bin/sh

# This is an adapted version of
# https://scan.coverity.com/scripts/travisci_build_coverity_scan.sh
# that doesn't try to import stuff from git.

set -e

# Environment check
# COVERITY_SCAN_PROJECT_NAME and COVERITY_SCAN_TOKEN are available on Project Settings page on scan.coverity.com
[ -z "$COVERITY_SCAN_PROJECT_NAME" ] && echo "ERROR: COVERITY_SCAN_PROJECT_NAME must be set" && exit 1
[ -z "$COVERITY_SCAN_NOTIFICATION_EMAIL" ] && echo "ERROR: COVERITY_SCAN_NOTIFICATION_EMAIL must be set" && exit 1
[ -z "$COVERITY_SCAN_BRANCH_PATTERN" ] && echo "ERROR: COVERITY_SCAN_BRANCH_PATTERN must be set" && exit 1
[ -z "$COVERITY_SCAN_BUILD_COMMAND" ] && echo "ERROR: COVERITY_SCAN_BUILD_COMMAND must be set" && exit 1
[ -z "$COVERITY_SCAN_TOKEN" ] && echo "ERROR: COVERITY_SCAN_TOKEN must be set" && exit 1

PLATFORM=$(uname)
TOOL_ARCHIVE=/tmp/cov-analysis-${PLATFORM}.tgz
TOOL_URL=https://scan.coverity.com/download/${PLATFORM}
TOOL_BASE=/tmp/coverity-scan-analysis
UPLOAD_URL="https://scan.coverity.com/builds"
SCAN_URL="https://scan.coverity.com"

# If the cache is enabled coverity won't get any data from the compiler run.
export CCACHE_DISABLE=1

# Do not run on pull requests
if [ "${TRAVIS_PULL_REQUEST}" = "true" ]; then
  echo -e "\033[33;1mINFO: Skipping Coverity Analysis: branch is a pull request.\033[0m"
  exit 0
fi

# Verify this branch should run
IS_COVERITY_SCAN_BRANCH=$(ruby -e "puts '${TRAVIS_BRANCH}' =~ /\\A$COVERITY_SCAN_BRANCH_PATTERN\\z/ ? 1 : 0")
if [ "$IS_COVERITY_SCAN_BRANCH" = "1" ]; then
  echo -e "\033[33;1mCoverity Scan configured to run on branch ${TRAVIS_BRANCH}\033[0m"
else
  echo -e "\033[33;1mCoverity Scan NOT configured to run on branch ${TRAVIS_BRANCH}\033[0m"
  exit 1
fi

# Verify upload is permitted
AUTH_RES=$(curl -s --form project="$COVERITY_SCAN_PROJECT_NAME" --form token="$COVERITY_SCAN_TOKEN" $SCAN_URL/api/upload_permitted)
if [ "$AUTH_RES" = "Access denied" ]; then
  echo -e "\033[33;1mCoverity Scan API access denied. Check COVERITY_SCAN_PROJECT_NAME and COVERITY_SCAN_TOKEN.\033[0m"
  exit 1
else
  AUTH=$(echo $AUTH_RES | ruby -e "require 'rubygems'; require 'json'; puts JSON[STDIN.read]['upload_permitted']")
  if [ "$AUTH" = "true" ]; then
    echo -e "\033[33;1mCoverity Scan analysis authorized per quota.\033[0m"
  else
    WHEN=$(echo $AUTH_RES | ruby -e "require 'rubygems'; require 'json'; puts JSON[STDIN.read]['next_upload_permitted_at']")
    echo -e "\033[33;1mCoverity Scan analysis NOT authorized until $WHEN.\033[0m"
    exit 0
  fi
fi

# Download Coverity Scan Analysis Tool
echo -e "\033[33;1mDownloading Coverity Scan Analysis Tool...\033[0m"
wget -nv -O coverity_tool.tgz $TOOL_URL --post-data "project=$COVERITY_SCAN_PROJECT_NAME&token=$COVERITY_SCAN_TOKEN"

# Extract Coverity Scan Analysis Tool
echo -e "\033[33;1mExtracting Coverity Scan Analysis Tool...\033[0m"
tar xzf coverity_tool.tgz


TOOL_DIR=$(find -type d -name 'cov-analysis*')
export PATH=$TOOL_DIR/bin:$PATH

# Build
echo -e "\033[33;1mRunning Coverity Scan Analysis Tool...\033[0m"
COV_BUILD_OPTIONS="--return-emit-failures --parse-error-threshold 85"
cmake .
COVERITY_UNSUPPORTED=1 cov-build --dir cov-int $COV_BUILD_OPTIONS make

# Upload results
echo -e "\033[33;1mTarring Coverity Scan Analysis results...\033[0m"
RESULTS_ARCHIVE=analysis-results.tgz
tar czf $RESULTS_ARCHIVE cov-int
SHA=$(git rev-parse --short HEAD)

echo -e "\033[33;1mUploading Coverity Scan Analysis results...\033[0m"
response=$(curl \
  --silent --write-out "\n%{http_code}\n" \
  --form project=$COVERITY_SCAN_PROJECT_NAME \
  --form token=$COVERITY_SCAN_TOKEN \
  --form email=$COVERITY_SCAN_NOTIFICATION_EMAIL \
  --form file=@$RESULTS_ARCHIVE \
  --form version=$SHA \
  --form description="Travis CI build" \
  $UPLOAD_URL)
status_code=$(echo "$response" | sed -n '$p')
if [ "$status_code" != "201" ]; then
  TEXT=$(echo "$response" | sed '$d')
  echo -e "\033[33;1mCoverity Scan upload result: $status_code $TEXT.\033[0m"
  exit 0
fi
exit 0
