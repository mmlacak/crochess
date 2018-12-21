#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# from scene import Scene
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
#from scene_nineteen import SceneNineteenMixin
#from scene_hemeras_dawn import SceneHemerasDawnMixin
#from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
#from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin


#class SceneMix(SceneCroatianTiesMixin, \
#               SceneMayanAscendancyMixin, \
#               SceneAgeOfAquariusMixin, \
#               SceneMirandasVeilMixin, \
#               SceneNineteenMixin, \
#               SceneHemerasDawnMixin, \
#               SceneTamoanchanRevisitedMixin, \
#               SceneConquestOfTlalocanMixin):

class SceneMix(SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin, \
               SceneAgeOfAquariusMixin, \
               SceneMirandasVeilMixin):

    def _get_recent_scene_method_names(self):
        return  [
                    'scn_mv_03_move_wave_finished', \
                    'scn_mv_20_wave_activation_by_pawn', \
                ]

    def _get_all_scene_method_names(self):
        return [ n for n in dir(self) if n.startswith('scn_') ] # or n.startswith('move_')

    def _get_attributes(self, names):
        return [ getattr(self, a) for a in names ]

    def get_recent_scene_methods(self):
        return self._get_attributes( self._get_recent_scene_method_names() )

    def get_all_scene_methods(self):
        return self._get_attributes( self._get_all_scene_method_names() )
