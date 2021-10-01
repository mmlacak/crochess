#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.


class SceneMixin:

    def _get_recent_scene_method_names(self):
        raise NotImplementedError('_get_recent_scene_method_names')

    def _get_all_scene_method_names(self, prefix='scn_'):
        return [ n for n in dir(self) if n.startswith(prefix) ] # or n.startswith('move_')

    def _get_attributes(self, names):
        return [ getattr(self, a) for a in names ]

    def get_recent_scene_methods(self):
        return self._get_attributes( self._get_recent_scene_method_names() )

    def get_all_scene_methods(self, prefix='scn_'):
        return self._get_attributes( self._get_all_scene_method_names(prefix=prefix) )
