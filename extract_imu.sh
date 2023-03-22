#!/bin/bash

Rscript step1-synchronization/extract_imu_from_zip.R $1 $2
Rscript project_status/generate_dashboard.R
quarto render
git add .
git commit -m "Updated Dashboard"
git push