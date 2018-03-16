package owe.events

trait Tracker {
  def post(event: Event)
}
