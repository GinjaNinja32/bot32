
zeroone() {
	case "$1" in
		0) echo "";;
		1) echo "$1 $2";;
		*) echo "$1 $3";;
	esac
}
concat() {
	if [[ "$2" != "" ]]; then
		echo "$1, $2"
	else
		echo "$1"
	fi
}
timediff() {
	local t="$1"

	local d="$(zeroone "$((t/60/60/24))" day days)"
	local h="$(zeroone "$((t/60/60%24))" hour hours)"
	local m="$(zeroone "$((t/60%60))" minute minutes)"
	local s="$(zeroone "$((t%60))" second seconds)"

	if [[ "$d" != "" ]]; then concat "$d" "$h"
	elif [[ "$h" != "" ]]; then concat "$h" "$m"
	elif [[ "$m" != "" ]]; then concat "$m" "$s"
	elif [[ "$s" != "" ]]; then echo "$s"
	else echo "???"
	fi
}

rev="$1"
branch="$3"
github="$4"

cd "$2"
git fetch origin > /dev/null 2> /dev/null

ancestor=false
descendant=false

if git merge-base --is-ancestor origin/"$branch" "$rev" >/dev/null 2>/dev/null; then
	descendant=true
fi

if git merge-base --is-ancestor $rev origin/"$branch" >/dev/null 2>/dev/null; then
	ancestor=true
fi


if $ancestor && $descendant; then
	echo "Current server revision is equal to $branch; no update required"
elif $ancestor; then
	commit_count="$(git log --oneline $rev..origin/"$branch" | wc -l)"
	if [[ $commit_count == 1 ]]; then commit_count="1 commit"
	else commit_count="$commit_count commits"
	fi
	dev_time=$(git log origin/"$branch" -n 1 --format=%ct) # Unix timestamps
	rev_time=$(git log $rev -n 1 --format=%ct)
	time_difference=$(timediff $(($dev_time - $rev_time)))
	echo "Current server revision is $time_difference ($commit_count) behind $branch; update required - https://$github/compare/$rev...$branch"
elif $descendant; then
	echo "Current server revision is descendant of $branch; is the server ahead of GitHub?"
else
	echo "Current server revision has unknown status. Possibly $branch and the server have diverged."
fi
