# Using requests directly

import requests
import os
import logging

file_containing_urls = "worldpop_file_urls.md"

def get_country_folder(filename):
    if filename.startswith("mwi_"):
        return "Malawi_Covs"
    elif filename.startswith("tza_"):
        return "Tanzania_Covs"
    elif filename.startswith("moz_"):
        return "Mozambique_Covs"
    elif filename.startswith("zmb_"):
        return "Zambia_Covs"
    else:
        return None

def download_file(url, out_path):
    # Send HTTP GET request to the URL, streaming the response
    response = requests.get(url, stream=True)
    # Raise an error if the request failed (e.g., 404 or 500)
    response.raise_for_status()
    # Create the output directory if it doesn't exist
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    # Open the output file in binary write mode
    with open(out_path, 'wb') as f:
        # Write the file in chunks to avoid loading the whole file into memory
        for chunk in response.iter_content(chunk_size=8192):
            f.write(chunk)
    # Log confirmation of download
    logging.info(f"Downloaded to {out_path}")

def main():
    # Set up logging to file
    logging.basicConfig(filename='covariate_download.log',
                        filemode='a',
                        format='%(asctime)s %(levelname)s: %(message)s',
                        level=logging.INFO)

    # Path to the markdown file containing URLs
    md_path = file_containing_urls
    # Base directory for covariate data
    base_dir = os.path.join("data", "covariate_data")
    # Open the markdown file for reading
    with open(md_path, 'r', encoding='utf-8') as md:
        # Iterate over each line in the file
        for line in md:
            # Remove whitespace from the line
            url = line.strip()
            # Skip lines that do not start with 'http' (not a URL)
            if not url.startswith("http"):
                continue
            # Extract the filename from the URL
            filename = url.split("/")[-1]
            # Determine the country folder based on filename prefix
            country_folder = get_country_folder(filename)
            # Skip if the filename does not match any country prefix
            if not country_folder:
                continue
            # Build the output directory path for the country
            out_dir = os.path.join(base_dir, country_folder)
            # Build the full output file path
            out_path = os.path.join(out_dir, filename)
            # If the file already exists, log and skip download
            if os.path.exists(out_path):
                logging.info(f"Exists: {out_path}")
                continue
            try:
                # Attempt to download the file
                download_file(url, out_path)
            except Exception as e:
                # Log any errors encountered during download
                logging.error(f"Failed to download {url}: {e}")

if __name__ == "__main__":
    main()