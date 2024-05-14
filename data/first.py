import requests

url = 'https://api.github.com/search/repositories'
params = {
    'q': 'language:COBOL',
    'sort': 'stars',  # Sorting by stars to get popular repositories
    'order': 'desc',
    'per_page': 10    # Number of repositories to fetch
}

response = requests.get(url, params=params)
if response.status_code == 200:
    repos = response.json().get('items', [])
    for repo in repos:
        print(f"Repository Name: {repo['name']}")
        print(f"URL: {repo['html_url']}")
        print(f"Description: {repo['description']}\n")
else:
    print('Failed to retrieve data from GitHub API')
