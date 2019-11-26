﻿using System;
using AARC.Mesh.Interface;
using AARC.Mesh.Model;

namespace AARC.Graph.Test
{
    public static class MeshFactory
    {
        public static MeshQueueMarshal MeshActionFactory(string name) 
        {
            switch (name)
            {
                case @"testcloserandom":
                    return new Method1QueueTransform();
                case @"closepriceservice":
                    return new ClosePriceQueueTransform();
                case @"randompriceservice":
                    return new RandomPriceQueueTransform();
                default:
                    return new QueueListener(new string[] { name });
            }
        }
    }
}
