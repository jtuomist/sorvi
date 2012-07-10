# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


setClass("WMSLayer", representation(name = "character",
                                    group = "character",
                                    layer = "character"))

setClass("WMS", representation(base.url = "character",
                               data = "XMLNode",
                               layers = "list"))

setMethod("initialize", "WMS", function(.Object, base.url) {
  
  # TODO: how does one check if arguments are provided?
  # TODO: how to check if response is valid?
  # TODO: which subset of GetCapabilities response should be mapped to the 
  #       object?
  
  # 
  
  wms.data <- GetCapabilities(base.url)
  
  .Object@base.url <- base.url
  .Object
})
