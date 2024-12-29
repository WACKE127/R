from django.db import models

class Item(models.Model):
    item_id = models.CharField(max_length=255, primary_key=True)
    item_name = models.CharField(max_length=255, null=True, blank=True)
    listing_title = models.CharField(max_length=255, null=True, blank=True)
    seller = models.CharField(max_length=255, null=True, blank=True)
    msrp = models.CharField(max_length=50, null=True, blank=True)
    listed_price = models.CharField(max_length=50, null=True, blank=True)
    movement = models.CharField(max_length=255, null=True, blank=True)
    brand = models.CharField(max_length=255, null=True, blank=True)
    condition = models.CharField(max_length=255, null=True, blank=True)
    department = models.CharField(max_length=255, null=True, blank=True)
    sales = models.CharField(max_length=255, null=True, blank=True)

    def __str__(self):
        return self.item_name or self.listing_title or "Item"
