unbind-key -n C-a
set -g prefix ^A
set -g prefix2 ^A
bind a send-prefix
bind-key -T root F1 select-window -t :=0
bind-key -T root F2 select-window -t :=1
bind-key -T root F3 select-window -t :=2
bind-key -T root F4 select-window -t :=3
bind-key -T root F5 select-window -t :=4
