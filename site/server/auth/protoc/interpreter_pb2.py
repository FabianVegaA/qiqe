# -*- coding: utf-8 -*-
# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: interpreter.proto
"""Generated protocol buffer code."""
from google.protobuf import descriptor as _descriptor
from google.protobuf import descriptor_pool as _descriptor_pool
from google.protobuf import symbol_database as _symbol_database
from google.protobuf.internal import builder as _builder
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()




DESCRIPTOR = _descriptor_pool.Default().AddSerializedFile(b'\n\x11interpreter.proto\x12\x0binterpreter\"\"\n\x12InterpreterRequest\x12\x0c\n\x04\x63ode\x18\x01 \x01(\t\"D\n\x13InterpreterResponse\x12\x0e\n\x06output\x18\x01 \x01(\t\x12\r\n\x05\x65rror\x18\x02 \x01(\t\x12\x0e\n\x06status\x18\x03 \x01(\x08\x32^\n\x12InterpreterService\x12H\n\x03run\x12\x1f.interpreter.InterpreterRequest\x1a .interpreter.InterpreterResponseb\x06proto3')

_globals = globals()
_builder.BuildMessageAndEnumDescriptors(DESCRIPTOR, _globals)
_builder.BuildTopDescriptorsAndMessages(DESCRIPTOR, 'interpreter_pb2', _globals)
if _descriptor._USE_C_DESCRIPTORS == False:
  DESCRIPTOR._options = None
  _globals['_INTERPRETERREQUEST']._serialized_start=34
  _globals['_INTERPRETERREQUEST']._serialized_end=68
  _globals['_INTERPRETERRESPONSE']._serialized_start=70
  _globals['_INTERPRETERRESPONSE']._serialized_end=138
  _globals['_INTERPRETERSERVICE']._serialized_start=140
  _globals['_INTERPRETERSERVICE']._serialized_end=234
# @@protoc_insertion_point(module_scope)
