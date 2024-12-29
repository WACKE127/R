# watches/management/commands/update_items.py
import os
import requests
from django.core.management.base import BaseCommand
from eBayProject.models import Item

class Command(BaseCommand):
    help = 'Fetch eBay watches and insert/update local DB'

    def handle(self, *args, **options):
        token = os.getenv(eBay)
        
        print(token)
        if not token:
            self.stdout.write(self.style.ERROR("No eBay token found in environment."))
            return

        # Fetch data
        items = self.fetch_watches(token)

        # Insert or update the database
        for i in items:
            Item.objects.update_or_create(
                item_id=i.get("itemId"),
                defaults={
                    "item_name": i.get("title"),
                    "listing_title": i.get("title"),
                    "seller": i.get("seller", {}).get("username"),
                    "msrp": i.get("price", {}).get("value"),
                    "listed_price": i.get("price", {}).get("value"),
                    "movement": i.get("additionalProductIdentities", [{}])[0]
                                  .get("productAspect", {})
                                  .get("movement"),
                    "brand": i.get("brand"),
                    "condition": i.get("condition"),
                    "department": i.get("department"),
                    "sales": None,
                }
            )

        self.stdout.write(self.style.SUCCESS("Successfully updated items."))

    def fetch_watches(self, token, query="watches", limit=10):
        url = "https://api.ebay.com/buy/browse/v1/item_summary/search"
        headers = {"Authorization": f"Bearer {token}"}
        params = {"q": query, "limit": limit}
        r = requests.get(url, headers=headers, params=params)
        return r.json().get("itemSummaries", [])
