function fish_prompt
    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname -s)
    end
    set_color -b cyan -o white
    echo -n (date "+%H%M ")
    set_color -b green -o white
    echo -n -s "$__fish_prompt_hostname>"
    set_color normal
    echo -n ' '
end
