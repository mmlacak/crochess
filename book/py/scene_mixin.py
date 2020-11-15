#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


class SceneMixin:

    def _get_recent_scene_method_names(self):
        raise NotImplementedError('_get_recent_scene_method_names')

    def _get_all_scene_method_names(self, prefix='scn_', contains=None):
        return [ n for n in dir(self) if n.startswith(prefix) and (contains is None or any( [c in n for c in contains] )) ] # or n.startswith('move_')

    def _get_attributes(self, names):
        return [ getattr(self, a) for a in names ]

    def get_recent_scene_methods(self):
        return self._get_attributes( self._get_recent_scene_method_names() )

    def get_all_scene_methods(self, prefix='scn_', contains=None):
        return self._get_attributes( self._get_all_scene_method_names(prefix=prefix, contains=contains) )
