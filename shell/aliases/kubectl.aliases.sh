alias k='kubectl'

alias kg='kubectl get'
alias kgp='kubectl get pods'
alias kgs='kubectl get svc'
alias kgd='kubectl get deployments'
alias kga='kubectl get all'

alias kd='kubectl describe'
alias kdp='kubectl describe pod'

alias kl='kubectl logs'
alias klf='kubectl logs -f'

alias kex='kubectl exec -it'
alias kaf='kubectl apply -f'
alias kdel='kubectl delete'
alias kpf='kubectl port-forward'
alias ktop='kubectl top'

# context / namespace switching (arg appends: `kns mynamespace`, `kctx my-cluster`)
alias kctx='kubectl config use-context'
alias kctxs='kubectl config get-contexts'
alias kns='kubectl config set-context --current --namespace'
