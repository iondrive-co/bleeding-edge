package org.bleedingedge.domain

/**
 * Message types for network communication in the distributed file synchronization protocol.
 *
 * @since 2.0.0
 */
enum MessageType:
  /** Broadcast a state snapshot to all peers in the cluster */
  case StateBroadcast

  /** Request state information from peers */
  case StateRequest
