# Check if a filename argument is provided
if [ -z "$1" ]; then
  echo "Usage: bash $0 <filename.tts>"
  exit 1
fi

swipl -f src/finalExecute.pl -g run -g halt -- "$1"