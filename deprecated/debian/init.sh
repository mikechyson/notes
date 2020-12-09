# install software
apt install -y git emacs gnome ibus-libpinyin frescobaldi


sed -i 's/#alias ll/alias ll/g' ~/.bashrc


cd
git clone git@github.com:hackchyson/notes.git 
git clone git@github.com:hackchyson/ai.git 

sh ~/notes/git/git-init


for i in `dpkg -l | grep game | grep -v support  | grep -v gnome-games |grep -v libgme | awk '{print $2}'`; do apt remove -y $i ; done
