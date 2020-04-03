import logging
import math
import pathlib
import time
from typing import Awaitable, Optional

import pkg_resources
import tornado.escape
import tornado.web
from mopidy import config, ext

__version__ = pkg_resources.get_distribution("Mopidy-Moparty").version

logger = logging.getLogger(__name__)

alive_clients = dict()
skip_votes = set()


def filter_alive():
    global alive_clients
    new_clients = dict()
    for key, value in alive_clients.items():
        if value > time.time() - 60:
            new_clients[key] = time.time()
    alive_clients = new_clients


class SkipRequestHandler(tornado.web.RequestHandler):
    def prepare(self) -> Optional[Awaitable[None]]:
        self.set_header("Content-Type", "application/json")
        return super().prepare()

    def initialize(self, core):
        self.core = core

    def post(self):
        global alive_clients
        global skip_votes

        data = tornado.escape.json_decode(self.request.body)
        alive_clients[data["client_id"]] = time.time()

        filter_alive()

        new_votes = set()
        current_track = self.core.playback.get_current_track().get()
        if current_track is None:
            self.write(tornado.escape.json_encode({"success": False}))
            return

        current_uri = current_track.uri
        skip_votes.add((current_uri, data["client_id"]))

        for uri, client_id in skip_votes:
            if uri == current_uri and client_id in alive_clients:
                new_votes.add((uri, client_id))
        skip_votes = new_votes

        if len(skip_votes) >= math.ceil(len(alive_clients) / 2):
            self.core.playback.next()
            self.write(tornado.escape.json_encode({"success": True}))
        else:
            self.write(
                tornado.escape.json_encode(
                    {
                        "need": math.ceil(len(alive_clients) / 2)
                        - len(skip_votes)
                    }
                )
            )


class AliveRequestHandler(tornado.web.RequestHandler):
    def get(self):
        filter_alive()
        self.write(tornado.escape.json_encode({"alive": len(alive_clients)}))

    def post(self):
        data = tornado.escape.json_decode(self.request.body)
        alive_clients[data["client_id"]] = time.time()


def mopidy_app_factory(config, core):
    return [
        ("/api/skip", SkipRequestHandler, {"core": core}),
        ("/api/alive", AliveRequestHandler, {}),
        (
            "/(moparty.min.js)",
            tornado.web.StaticFileHandler,
            {"path": str(pathlib.Path(__file__).parent / "static")},
        ),
        (
            "/img/(.*)",
            tornado.web.StaticFileHandler,
            {"path": str(pathlib.Path(__file__).parent / "static" / "img")},
        ),
        (
            "/.*()",
            tornado.web.StaticFileHandler,
            {
                "path": str(
                    pathlib.Path(__file__).parent / "static" / "index.html"
                )
            },
        ),
    ]


class Extension(ext.Extension):

    dist_name = "Mopidy-Moparty"
    ext_name = "moparty"
    version = __version__

    def get_default_config(self):
        return config.read(pathlib.Path(__file__).parent / "ext.conf")

    def setup(self, registry):
        registry.add(
            "http:app", {"name": self.ext_name, "factory": mopidy_app_factory},
        )
