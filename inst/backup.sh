
# first backup
sudo mariadb-backup --backup --target-dir=/ds/scidbbackup/mariabackup/currentbk  --parallel=20 --rsync
sudo mariadb-backup --prepare --target-dir=/ds/scidbbackup/mariabackup/currentbk   
