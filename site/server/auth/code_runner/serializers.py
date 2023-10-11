from rest_framework import serializers


class CodeRunnerSerializer(serializers.Serializer):
    code = serializers.CharField(max_length=1000)
