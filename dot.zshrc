case "$TERM"; in
		emacs)
			NO_COLORS=true
		;;
		*)
			NO_COLORS=
		;;
esac

for file in `/bin/ls $HOME/.zsh/rc/* | grep -v ".*~"`; do
		source $file
done
