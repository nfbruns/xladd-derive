﻿using System;
using System.Collections.Generic;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using AARC.Model.Interfaces;
using log4net;
using AARC.Repository.Interfaces;

// For more information on enabling Web API for empty projects, visit https://go.microsoft.com/fwlink/?LinkID=397860

namespace aarcweb.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class EODController : ControllerBase
    {
        private ILog _logger = LogManager.GetLogger(typeof(EODController));
        private readonly IMarketDataRepository _marketDataProvider;

        public EODController(IMarketDataRepository marketDataProvider)
        {
            _logger.Debug($"{this.GetType().Name} created {marketDataProvider?.GetType().Name} provided");
            _marketDataProvider = marketDataProvider;
        }

        [HttpGet]
        public string Get()
            => $"{this.GetType().Name}-{_marketDataProvider.GetInfo()} {DateTime.Now:yyyyMMdd-HHmmss}";


        [HttpGet("Stocks")]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public IList<string> Stocks() => _marketDataProvider.GetTickers();

        [HttpGet("Status")]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public IList<Tuple<string, uint>> Status() => _marketDataProvider.GetStatus();

        [HttpGet("Info/{symbol}")]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        //public async Task<ActionResult<IStockInfo>> GetInfoByStockAsync(string stock)
        public IAarcInstrument SymbolInfo(string symbol)
        {
            var info = _marketDataProvider.GetInfo(new List<string> { symbol });

            var stockInfo = info[symbol];
            return stockInfo;
        }

        [HttpGet("History/{symbol}/{from}/{to}")]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public IAarcPrice SymbolHistory(string symbol, uint from, uint to)
        {
            var results = _marketDataProvider.GetClosingPrices(new List<string> { symbol }, from, to);

            IAarcPrice history = results[symbol];
            return history;
        }
    }
}
