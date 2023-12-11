# HydroVu API pull

This folder contains a basic API script to pull data from the HydroVu API programatically.

Right now, this script requires functions from the HydroVuR package - and I've had to update a few of the functions to work for our purposes - those are the two 'hv\_....R' scripts. Ideally, B will integrate these updates into the forked GH package and then we can just use the package functions and not need to call in the two 'hv\_....R' scripts.

## Requirements

In order for this workflow to work you need to add the API Access Credentials from the [HydroVu People & Permissions](https://www.hydrovu.com/#/company-dashboard/users/list) page (see bottom 'Manage API Access Credentials'). To create your secret, add a client id of your choosing (please make sure it's clear we know who the credential belongs to), then click the '+ New Client Secret'.

You should store these credentials in a file called 'credentials.yml' as indicated in the file 'CopyYourCreds.yml' - 'credentials.yml' is NOT tracked in GitHub, but 'CopyYourCreds.yml' IS. If you commit our API Access Credentials in the tracked file ('CopyYourCreds.yml'), the internet now knows how to access all of our data. Don't do that. :) You commit history persists as long as your repository exists. If you happen to commit your API secret to GitHub, please delete your existing API client secret and create a new one using the process outlined above (you can keep your client id).

## General Workflow

1)  fill in your API credentials. Follow the steps in **Requirements** to create and copy your Client ID and Secret, then follow the directions in the header of 'CopyYourCreds.yml' to update the template to include your API access info. **MAKE SURE YOU RENAME THE FILE TO 'credentials.yml' IMMEDIATELY UPON ENTERING YOUR API CREDENTIALS.**

2)  Run the 'HydroVu_API_pull.Rmd' script. All further directions are in this file. Start/Stop dates and file export name are currently hardcoded in here. Will need to update when we decide how to deploy this script.
