import subprocess
# set working directory to contain uber_buckets, yellow_buckets, green_buckets
subprocess.call("Rscript parse.R", shell=True)
