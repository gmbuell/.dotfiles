# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/bin:/usr/lib/google-golang/bin:/usr/local/buildtools/java/jdk/bin:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH

export EDITOR='emacsclient --alternate-editor=""'
export P4EDITOR=$EDITOR
bindkey -e

fpath=(/google/src/files/head/depot/google3/devtools/blaze/scripts/zsh_completion $fpath)

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache/

alias zshconfig="$EDITOR ~/.zshrc"
alias sslook=/home/build/static/projects/sstable/sstable_lookup
alias f1=/home/build/static/projects/storage/f1/tools/f1-sql-prod
alias saw=/google/data/ro/projects/logs/saw

function counterdiff() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    blaze clean && blaze build -c opt contentads/usertargeting/kansas/mr/mr-kansas_iba contentads/usertargeting/testing/blackbox_test/ks_combined && blaze-bin/contentads/usertargeting/testing/blackbox_test/ks_combined --run_counter_diff --use_custom_binary --backup_start_row=dbl:2200000000000000 --backup_limit_row=dbl:2200010000000000 --email=${USER}@google.com --run_ks_combined=daily --alsologtostderr --borguser=content-ads-icm-testing --gfs_user=content-ads-icm-testing
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function dryrun() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    blaze clean && blaze build -c opt contentads/usertargeting/kansas/mr/mr-kansas_icm && borgcfg --borguser=content-ads-icm production/borg/content-ads-icm/kansas/ks_exp.borg --vars=user=content-ads-icm,is_debug_run=true,custom_bin=blaze-bin/contentads/usertargeting/kansas/mr/mr-kansas_icm up -skip_confirmation=true
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function icmcookie() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    blaze run -- contentads/experiments/tools:find_cookie_remainders_for_experiment --experiment_id 318471429
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function expconfigcheck() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    /home/build/google3/contentads/experiments/tools/mendel/ca_mendel_client.sh update
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function updatetools() {
  cd ~/tools/google3
  git5 sync && blaze build -c opt //configlang/gcl/interpreter:gcl2a
  popd
}

function bclrepl() {
  ~/tools/google3/borg/configs/tools/repl.sh $1
}

function gclrepl() {
  ~/tools/google3/configlang/gcl/tools/repl.sh $1
}

function mailgus() {
  if [[ $(git diff --shortstat 2> /dev/null | tail -n1) == "" ]]; then
    git5 mail -m katsiapis --cc mad-quality,mad-audience,gmob-experiments --no-body
  else
    echo "Commit changes first"
  fi
}

function mailexp() {
  if [[ $(git diff --shortstat 2> /dev/null | tail -n1) == "" ]]; then
    exp_oncall=$(oncall 513016)
    git5 mail -m $exp_oncall,gmob-experiments --cc mad-quality,mad-audience --no-body
  else
    echo "Commit changes first"
  fi
}

function oncall() {
  ~/tools/google3/experimental/users/gmbuell/bin/web_extract.rb "rotation.googleplex.com/status?id=$1" "#rotations-primary .rotations-box-user"
}

function icm_oncall() {
  oncall 2545016
}

function exp_oncall() {
  oncall 513016
}

function mailicm() {
  if [[ $(git diff --shortstat 2> /dev/null | tail -n1) == "" ]]; then
    git5 mail -m $(icm_oncall) --cc mad-audience,icm-quality+reviews,tingliu --no-body
  else
    echo "Commit changes first"
  fi
}

function expcheck() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) == "" ]] && git5 export --sq --tap-project=contentads_experiments,gmob.presubmit,gmob_experiment_config,gmob.e2e
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function expchecklast() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) == "" ]] && git5 export --sq --tap-project=contentads_experiments,gmob.presubmit,gmob_experiment_config,gmob.e2e --tap-options="rerun=last"
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function expsubmit() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) == "" ]] && git5 submit --sq --tap-project=contentads_experiments,gmob.presubmit,gmob_experiment_config,gmob.e2e
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function expsubmitrr() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) == "" ]] && git5 submit --sq --tap-project=contentads_experiments,gmob.presubmit,gmob_experiment_config,gmob.e2e --tap-options="rerun=last"
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function gmbtest() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    # Do stuff here
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function localmixer() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    /google/src/head/depot/google3/devtools/blaze/scripts/blaze-run.sh contentads/gmob/testing/gmobenv/gmobenv -- --gmobenv_jobs=mixer --gmobenv_cell=ic --gmobenv_user=content-ads-icm
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function requesttool() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    /google/src/head/depot/google3/devtools/blaze/scripts/blaze-run.sh contentads/gmob/tools/gmobrequest:gmobrequest
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function fix_cl() {
  pwd | grep -o '.*google3$'
  IN_GOOGLE3=$?
  if [[ $IN_GOOGLE3 -eq 0 ]] || {cd `pwd | grep -o '.*google3'`} && [[ $? -eq 0 ]]; then
    targets=$((for fullname in $(blaze query "set($(git5 diff --relative --name-only))"); do blaze query "attr('srcs', $fullname, ${fullname//:*/}:*)"; done) | sort -u)
    echo $targets | xargs /google/src/head/depot/google3/devtools/maintenance/include_what_you_use/iwyu.py
    echo $targets | xargs /google/src/head/depot/google3/devtools/maintenance/include_what_you_use/iwyu.py --nosafe_headers
    echo $targets | xargs /home/build/devtools/blazeutils/fixdeps_main.par
    if [[ $IN_GOOGLE3 -ne 0 ]]; then
      popd
    fi
  fi
}

function list_citc() {
  g4 clients -u $USER | awk '{print $2}'
}

# function cleanup_client(client_arg) {
#   echo "*** Revering all pending changes ..."
#   p4 -c $client_arg revert -k //...

#   for cl in $(p4 -c $client_arg changes -s pending | awk '{print $2}'); do
#     echo "*** Removing CL" $cl "..."
#     p4 -c $client_arg change -d $cl
#   done

#   echo "*** Removing client ..."
#   g4 citc -d $client_arg
# }

#source /etc/bash_completion.d/g4d
G4D_LOCATION=${G4D_LOCATION:-/usr/lib/piper-client/internal}
typeset SHELL_TYPE=unknown
typeset proc_exe="$(basename $(readlink -f /proc/$$/exe))"
autoload -U bashcompinit
bashcompinit
SHELL_TYPE=zsh
typeset COMPLETION_STYLE=bash
function g4d() {
  typeset dir;
  dir=$("${G4D_LOCATION}/g4d_command_impl.sh" $*) && cd $dir;
}

function _g4d_bash::g4d_completion {
      # Must be in a subshell.
  COMPREPLY=( $(source "${G4D_LOCATION}/g4d_bash_completion_helper.sh") )
}
complete -o nospace -F _g4d_bash::g4d_completion g4d

# From google3/devtools/completions/git5
# function _git5_trampoline() {
#   COMPREPLY=( $(source \
#     "/usr/lib/git5/git5_bash_completion_helper.sh") )
# }
# complete -o nospace -F _git5_trampoline git5

CITC_ROOT=/google/src/cloud/${USER}/
SRCFS_HEAD_ROOT=/google/src/head/depot/
function citc_prompt_info() {
  local full_path=$(print -P '%d')
  if [ $full_path = $CITC_ROOT ]; then
    echo "citc:"
  elif [ $full_path = $SRCFS_HEAD_ROOT ]; then
    echo "srcfs-head:"
  fi
}

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
