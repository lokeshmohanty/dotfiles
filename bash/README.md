# My Bash Configuration Files

## Locations of system bashrc files

  01. /etc/skel/
  02. /etc/bash.bashrc

## TODO

  01. Add .prompt

## Tips
  
  - Use command `pushd <directory>` to push the directory to `dirs` stack and change directory
  - Use `pushd -n <directory>` to push to stack without changing directory
  - Use `pushd +N` to move to Nth directory of the stack and `pushd -N` to move from back of the stack
  - Use `dirs -l -v` to view the directories in the stack
  - Command `popd` acts same as `pushd` but it remove the directory from the stack
