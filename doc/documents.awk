
# from http://unix.stackexchange.com/questions/134212/extract-file-name-from-path-in-awk-program
function basename(file) {
    sub(".*/", "", file)
    return file
}

BEGIN{
    print "Documentation for "basename(ARGV[1])
    print "=================="
}

/^;{3}/{
    print "## "substr($0,4)"\n"
}
/^;{4}/{
    print "### "substr($0,5)"\n"
}
/^;{5}/{
    print "#### "substr($0,6)"\n"
}

/#\|/,/\|#/{
    if($0 ~ /#\|/ || $0 ~ /\|#/){
        print "\n"
    }else{
        print
    }
}

/defun/{
    name=$2
    $1=""
    $2=""
    print "* Function _"name"_ "$0"\n"
}

/defmacro/{
    name=$2
    $1=""
    $2=""
    print "* Macro _"name"_ "$0"\n"
}

/^ *"/,/.*"$/{
    # if($0 ~ /^ *"/){
    #     print "\n"
    # }
    match($0,/^ *"/)
    s1=RSTART
    s2=RLENGTH+1
    match($0,/" *$/)
    e1=RSTART
    if (e1==0){
        print "`"substr($0,s2)"`"
    }else if (s1==0){
        print "`"substr($0,s2,e1-s2-1)"`"
    }
    else{
        print "`"substr($0,s2,e1-s2)"`"
    }

    if ($0 ~ /.*"$/){
        print "\n"
    }
}
