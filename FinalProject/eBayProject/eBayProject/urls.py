from django.contrib import admin
from django.urls import path
from eBayProject.views import itemList

urlpatterns = [
    path('admin/', admin.site.urls),
    path('items/', itemList, name='itemList'),
]
