from django.shortcuts import render
from rest_framework.decorators import action
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from rest_framework.request import Request
from rest_framework.viewsets import ModelViewSet

from .serializers import CodeRunnerSerializer

import logging
from datetime import datetime


from django.views.decorators.csrf import csrf_exempt
from django.http import HttpResponse, JsonResponse
from rest_framework.parsers import JSONParser

import grpc

from protoc.interpreter_pb2 import InterpreterRequest
from protoc.interpreter_pb2_grpc import InterpreterServiceStub

from auth_server.settings import GRPC_SERVER


@csrf_exempt
def code_runner(request):
    match request.method:
        case "POST":
            serializer = CodeRunnerSerializer(data=JSONParser().parse(request))
            if not serializer.is_valid():
                return Response(status=400)
            with grpc.insecure_channel(GRPC_SERVER) as channel:
                res = InterpreterServiceStub(channel).run(InterpreterRequest(code=serializer.data["code"]))
                return JsonResponse({
                    "id": 0, # TODO: remove this, it's not needed
                    "result": res.output,
                    "error": res.error,
                    "status": res.status,
                    "createdAt": datetime.now().isoformat(),
                })
        case _:
            return Response(status=400)
