// This file is part of Substrate.

// Copyright (C) 2020-2021 Parity Technologies (UK) Ltd.
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

//! Middleware for RPC requests.

use futures::{future::Either, Future};
use jsonrpc_core::{
    FutureOutput, FutureResponse, Metadata, Middleware as RequestMiddleware, Request,
    Response,
};

/// Middleware for RPC calls
pub struct RpcMiddleware;

impl RpcMiddleware {
    /// Create an instance of middleware.
    ///
    /// - `metrics`: Will be used to report statistics.
    /// - `transport_label`: The label that is used when reporting the statistics.
    pub fn new() -> Self {
        RpcMiddleware {}
    }
}

impl Default for RpcMiddleware {
    fn default() -> Self {
        Self::new()
    }
}

impl<M: Metadata> RequestMiddleware<M> for RpcMiddleware {
    type Future = FutureResponse;
    type CallFuture = FutureOutput;

    fn on_request<F, X>(
        &self,
        request: Request,
        meta: M,
        next: F,
    ) -> Either<FutureResponse, X>
    where
        F: Fn(Request, M) -> X + Send + Sync,
        X: Future<Output = Option<Response>> + Send + 'static,
    {
        Either::Right(next(request, meta))
    }
}
