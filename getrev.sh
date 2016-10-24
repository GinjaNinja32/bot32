
current_server_rev=$1

cd ~/Baystation12
git fetch origin > /dev/null 2> /dev/null

ancestor=false
descendant=false

if git merge-base --is-ancestor origin/dev $current_server_rev; then
	descendant=true
fi

if git merge-base --is-ancestor $current_server_rev origin/dev; then
	ancestor=true
fi


if $ancestor && $descendant; then
	echo "Current server revision is equal to dev; no update required"
elif $ancestor; then
	echo "Current server revision is ancestor of dev; update required"
elif $descendant; then
	echo "Current server revision is descendant of dev; is the server ahead of GitHub?"
else
	echo "Current server revision has unknown status. Possibly dev and the server have diverged."
fi
