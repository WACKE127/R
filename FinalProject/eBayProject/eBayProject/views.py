from django.shortcuts import render
from .models import Item

def itemList(request):
    items = Item.objects.all().order_by('item_name')  # or any field
    return render(request, 'itemList.html', {'items': items})
