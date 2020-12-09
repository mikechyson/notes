for project in ~/notes ~/PycharmProjects/algorithms ~/PycharmProjects/computer_vision ~/PycharmProjects/python3 ~/PycharmProjects/packages/chyson
do
    cd $project
    echo =======================================================================
    echo $project
    git pull
    git push                    
done
