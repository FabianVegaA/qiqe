from django.urls import path
from code_runner import views

urlpatterns = [
    path("playground/", views.code_runner)
]
