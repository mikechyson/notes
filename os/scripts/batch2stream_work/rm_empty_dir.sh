#!/bin/bash

find /mfs/gn_enc_ftp -type d -size 0b -mmin +10 -exec rmdir {} \;
