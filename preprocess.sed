# Remove comments
/^#/d

# Remove trailing whitespaces
s/\s*$//

# Remove the opening called "Start"
/"Start"/d

# Remove empty lines
/^\s*$/d

# Join all lines that do not end in *
:a;/[^*]$/{N;s/\n//;ba}

# Normalize spaces
s/\s\s*/ /g

# Remove SCID ECO extension
s/^\([ABCDE][0-9][0-9]\)[a-z]/\1/

# Remove openings with move in the title, as they are extensions of known, shorter, ones.
/"[^"]*[1-9][0-9]*\.[^"]*"/d

## Remove line from opening title (FIXME: this occasionally removes a variation name that shows up after the first move) 
#s/\("[^"1-9]*\)[1-9][0-9]*\.[^"]*\("\)/\1\2/
#
## Clean up end of opening name
#s/("/"/
#s/\("("/"/

# Drop move numbers
s/[1-9][0-9]*\.//g

# Expand acronyms
s/\([^(]\)KGA/\1King's Gambit Accepted/g
s/\([^(]\)KGD/\1King's Gambit Declined/g
s/\([^(]\)KIA/\1King's Indian Attack/g
s/\([^(]\)QGA/\1Queen's Gambit Accepted/g
s/\([^(]\)QGD/\1Queen's Gambit Declined/g

# Kill moves
# s/^\([ABCDE][0-9][0-9] ".*"\).*$/\1/

# Replace (hopefully all) semicolons with slashes
s_; _/_g

# Delete lines with semicolons if any
/;/d

# Rewrite as easily parsable entries
s/^\([ABCDE][0-9][0-9]\) "\([^"]*\)" \([^*]*\) \*$/\1; \2; \3/

# Normalize spaces, again
s/\s\s*/ /g
