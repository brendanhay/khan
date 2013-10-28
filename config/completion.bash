_khan() {
    COMPREPLY=()
    local word="${COMP_WORDS[COMP_CWORD]}"

    if [ "$COMP_CWORD" -eq 1 ]; then
        COMPREPLY=( $(compgen -W "$(khan complete)" -- "$word") )
    else
        local command="${COMP_WORDS[1]}"
        local completions="$(khan complete --command "$command")"

        COMPREPLY=( $(compgen -W "$completions" -- "$word") )
    fi
}

complete -F _khan khan
