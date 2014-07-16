#!/bin/bash

echo $HOME $(id) $(pwd)

dist=/vagrant/dist-files
if [ ! -d $dist ]; then
    echo "$dist directory is not found"
    exit 1
fi

# jdk
jdk7=jdk-7u21-linux-x64.gz
java_home=/opt/jdk
if [ ! -d "$java_home" ]; then 
    echo installing jdk7

    sudo tar zxf $dist/$jdk7 -C /opt
    sudo ln -sf /opt/jdk1.7.0_21 $java_home
fi

# set up default java home and path variables
profile_jdk="/etc/profile.d/jdk.sh"
if [ ! -f "$profile_jdk" ]; then
    sudo cat > $profile_jdk <<EOF
export JAVA_HOME=$java_home
export PATH=$java_home/bin:$PATH
EOF
fi
